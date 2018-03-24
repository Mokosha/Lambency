{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lambency.Types (
  Vec2f, Vec3f, Vec4f, Quatf, Mat2f, Mat3f, Mat4f,
  Camera(..), CameraType(..), CameraViewDistance(..),
  LightVar(..), LightParams(..), LightType(..), Light(..),
  ShadowMap(..), ShadowTechnique(..),
  Shader(..), ShaderVarTy(..), ShaderValue(..), ShaderVar(..), ShaderMap,
  Texture(..), TextureSize(..), TextureFormat(..), FBOHandle, TextureHandle(..),
  MaterialVar(..), NormalModulation(..), ReflectionInfo(..), Material(..),
  RenderFlag(..), RenderObject(..), RenderAction(..), RenderActions(..),
  Sound, SoundCommand(..), SpriteFrame(..), Sprite(..),
  OutputAction(..),
  TimeStep,
  GameConfig(..), GameMonad(..), GameSession, GameTime,
  Game(..),
  GameWire, ContWire(..), ResourceContext, ResourceContextWire(..)
) where

--------------------------------------------------------------------------------

import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad.RWS.Strict
import Control.Monad.Reader
import qualified Control.Wire as W

import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Hashable
import Data.Profunctor
import Data.Time.Clock

import FRP.Netwire.Input.GLFW

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLRaw

import Prelude hiding ((.), id)

import Lambency.Utils
import qualified Lambency.Transform as XForm

import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import qualified Linear.Quaternion as Quat

import qualified Sound.OpenAL.AL as AL
--------------------------------------------------------------------------------

-- Vector Types

type Vec2i = V2 Int
type Vec2f = V2 Float
type Vec3f = V3 Float
type Vec4f = V4 Float

type Quatf = Quat.Quaternion Float

type Mat2f = M22 Float
type Mat3f = M33 Float
type Mat4f = M44 Float

--------------------------------------------------------------------------------

-- Cameras

data CameraViewDistance = CameraViewDistance {
  near :: Float,
  far :: Float
} deriving (Show, Eq)

-- !TODO! Choose better names for the Ortho planes.
data CameraType =
  Ortho {
    left :: Float,
    right :: Float,
    top :: Float,
    bottom :: Float
  }
  | Persp {
    fovY :: Float,
    aspect :: Float
  }
  deriving (Show)

data Camera = Camera XForm.Transform CameraType CameraViewDistance deriving(Show)

--------------------------------------------------------------------------------

-- Shaders

data ShaderVarTy = Matrix2Ty
                 | Matrix3Ty
                 | Matrix4Ty
                 | Matrix3ListTy
                 | Matrix4ListTy
                 | Vector2Ty
                 | Vector3Ty
                 | Vector4Ty
                 | Vector2ListTy
                 | Vector3ListTy
                 | Vector4ListTy
                 | IntTy
                 | IntListTy
                 | FloatTy
                 | FloatListTy
                 | TextureTy GLRaw.GLuint
                 | ShadowMapTy GLRaw.GLuint
                 deriving (Show, Eq, Ord)

data ShaderVar = Uniform ShaderVarTy GL.UniformLocation
               | Attribute ShaderVarTy GL.AttribLocation
               deriving (Show, Eq, Ord)

type ShaderVarMap = Map.Map String ShaderVar

data ShaderValue = Matrix2Val Mat2f
                 | Matrix3Val Mat3f
                 | Matrix4Val Mat4f
                 | Matrix3ListVal [Mat3f]
                 | Matrix4ListVal [Mat4f]
                 | Vector3Val Vec3f
                 | Vector4Val Vec4f
                 | Vector3ListVal [Vec3f]
                 | Vector4ListVal [Vec4f]
                 | IntVal Int
                 | IntListVal [Int]
                 | FloatVal Float
                 | FloatListVal [Float]
                 | TextureVal Texture
                 | ShadowMapVal ShadowMap
                 deriving (Show, Eq, Ord)

type ShaderMap = Map.Map String ShaderValue

data Shader = Shader GL.Program ShaderVarMap deriving(Show, Eq, Ord)

--------------------------------------------------------------------------------

-- Textures

newtype TextureSize = TexSize { getTextureSize :: Vec2i }
                      deriving(Show, Eq, Ord)

type FBOHandle = GL.FramebufferObject
data TextureHandle = TexHandle GL.TextureObject TextureSize
                     deriving(Show, Eq, Ord)
data TextureFormat = RGBA8 | RGB8 | Alpha8
                     deriving(Show, Eq, Ord, Enum, Bounded)

data Texture = Texture TextureHandle TextureFormat
             | RenderTexture TextureHandle FBOHandle
               deriving(Show, Eq, Ord)

--------------------------------------------------------------------------------

-- Lights

newtype LightVar a = LightVar { getLightVar :: (String, ShaderValue) }
                     deriving (Show, Eq, Ord)

instance Hashable (LightVar a) where
  hashWithSalt = hashUsing lightVarToStr
    where
      lightVarToStr (LightVar v) = fst v

data LightParams = LightParams {
  ambientColor :: LightVar (V3 Float),
  lightColor :: LightVar (V3 Float),
  lightIntensity :: LightVar Float
} deriving(Show, Eq, Ord)

instance Hashable LightParams where
  hashWithSalt s (LightParams x y z) =
    s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z

data LightType
  = SpotLight {
    spotLightDir :: LightVar (V3 Float),
    spotLightPos :: LightVar (V3 Float),
    spotLightCosCutoff :: LightVar Float
  }
  | DirectionalLight {
    dirLightDir :: LightVar (V3 Float)
  }
  | PointLight {
    pointLightPos :: LightVar (V3 Float)
  } deriving (Show, Eq, Ord)

instance Hashable LightType where
  hashWithSalt s (SpotLight x y z) =
    s `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z
  hashWithSalt s (DirectionalLight x) = s `hashWithSalt` x
  hashWithSalt s (PointLight x) = s `hashWithSalt` x

newtype ShadowMap = ShadowMap { getShadowmapTexture :: Texture }
                  deriving (Show, Eq, Ord)

data ShadowTechnique
  = ShadowTechnique'Simple
    deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Light
  = Light {
    lightParams :: LightParams,
    lightType :: LightType,
    lightShadowMap :: Maybe (ShadowMap, ShadowTechnique)
    }
  deriving (Show, Eq, Ord)

instance Hashable Light where
  hashWithSalt s (Light x y z) =
    hashUsing isJust (s `hashWithSalt` x `hashWithSalt` y) z

--------------------------------------------------------------------------------

-- Materials

newtype MaterialVar a = MaterialVar { getMatVar :: (String, Maybe ShaderValue) }
                      deriving (Show, Eq, Ord)

instance Hashable (MaterialVar a) where
  hashWithSalt s (MaterialVar (n, ms)) = hashUsing isJust (s `hashWithSalt` n) ms

data NormalModulation
  = BumpMap (MaterialVar Texture)
  | NormalMap (MaterialVar Texture)
  deriving (Show, Eq, Ord)

instance Hashable NormalModulation where
  hashWithSalt s (BumpMap v) = s `hashWithSalt` v
  hashWithSalt s (NormalMap v) = s `hashWithSalt` v

data ReflectionInfo
  = ReflectionInfo {
    indexOfRefraction :: MaterialVar Float,
    reflectionMap :: MaterialVar Texture,
    sharpness :: MaterialVar Float
  }
  deriving (Show, Eq, Ord)

instance Hashable ReflectionInfo where
  hashWithSalt s (ReflectionInfo a b c) =
    s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c

data Material
  = BlinnPhongMaterial {
    diffuseReflectivity :: MaterialVar (V3 Float),
    diffuseMap :: MaterialVar (Texture),

    specularExponent :: MaterialVar (Float),
    specularReflectivity :: MaterialVar (V3 Float),
    specularMap :: MaterialVar (Texture),

    ambientReflectivity :: MaterialVar (V3 Float),

    reflectionInfo :: Maybe (ReflectionInfo),

    normalMod :: Maybe (NormalModulation)
    }

    -- A textured sprite is a quad that has a texture on it
    -- The texture coordinates may be modulated based on the
    -- texture matrix
  | TexturedSpriteMaterial
    { spriteTextureMatrix :: MaterialVar (M33 Float)
    , spriteTexture :: MaterialVar Texture
    , spriteAlpha :: MaterialVar Float
    }

    -- A masked sprite is a quad that has a grayscale texture
    -- that represents an alpha mask. Everything else is colored
    -- based on the sprite color.
    -- The texture coordinates may be modulated based on the
    -- texture matrix.
  | MaskedSpriteMaterial {
    spriteMaskColor :: MaterialVar (V4 Float),
    spriteMaskMatrix :: MaterialVar (M33 Float),
    spriteMask :: MaterialVar Texture
    }
  | MinimalMaterial
  | NoMaterial
    deriving (Show, Eq, Ord)

instance Hashable Material where
  hashWithSalt s (BlinnPhongMaterial a b c d e f g h) =
    s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt`
    d `hashWithSalt` e `hashWithSalt` f `hashWithSalt` g `hashWithSalt` h
  hashWithSalt s (TexturedSpriteMaterial a b c) =
    s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
  hashWithSalt s (MaskedSpriteMaterial a b c) =
    s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
  hashWithSalt s (MinimalMaterial) = hashWithSalt s "MinimalMaterial"
  hashWithSalt s NoMaterial = hashWithSalt s "NoMaterial"

--------------------------------------------------------------------------------

-- Renderable Objects

data RenderFlag = Transparent
                | Text
                deriving (Show, Read, Ord, Eq, Enum)

data RenderObject = RenderObject
                    { material :: Material
                    , objectVars :: ShaderMap
                    , render :: Shader -> ShaderMap -> IO ()
                    , flags :: [RenderFlag]
                    , unloadRenderObject :: IO ()
                    }

data RenderAction = RenderObjects [RenderObject]
                  | RenderClipped RenderAction RenderAction
                  | RenderTransformed XForm.Transform RenderAction
                  | RenderCons RenderAction RenderAction

instance Monoid RenderAction where
  mempty = RenderObjects []
  mappend (RenderObjects []) x = x
  mappend x (RenderObjects []) = x
  mappend (RenderObjects x) (RenderObjects y) = RenderObjects (x ++ y)
  mappend x y = RenderCons x y

data RenderActions = RenderActions {
  renderScene :: RenderAction,
  renderUI :: RenderAction
}

instance Monoid RenderActions where
  mempty = RenderActions mempty mempty
  mappend (RenderActions a b) (RenderActions c d) =
    RenderActions (a `mappend` c) (b `mappend` d)

data SpriteFrame = SpriteFrame {
  offset :: V2 Float,
  spriteSize :: V2 Int,
  frameRO :: RenderObject
}

data Sprite = Sprite
  { spriteFrames :: CyclicList SpriteFrame
  , unloadSprite :: IO ()
  }

--------------------------------------------------------------------------------
--
-- Sound types
--
type Sound = AL.Source
data SoundCommand = StartSound | StopSound

--------------------------------------------------------------------------------

-- Output functions
--
-- These functions are used to create side effects from our game wires. In
-- general, the most common action will likely be to render something, however
-- we may also want to output sound or a debug string as well.
data OutputAction = LogAction String
                  | SoundAction Sound SoundCommand
                  | WireframeAction Bool

--------------------------------------------------------------------------------

data GameConfig = GameConfig {
  lastFrameTime :: Integer,  -- Picoseconds last frame took to render
  windowSize :: (Int, Int),  -- Size of the rendering window in pixels
  simpleSprite :: Sprite     -- A simple single-color sprite useful for fade-ins
                             -- and simple UI element backgrounds
  }

-- Game
data Game a = Game {
  mainCamera :: ContWire () Camera,
  dynamicLights :: [ContWire () Light],
  gameLogic :: ContWire a (Maybe a)
  }

--------------------------------------------------------------------------------

-- Game State

type TimeStep = W.Timed Float ()
type GameSession = W.Session IO TimeStep

newtype GameMonad a = GameMonad {
  nextFrame :: RWST GameConfig ([OutputAction], RenderActions) GLFWInputState IO a
} deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadFix
           , MonadPlus
           , MonadReader GameConfig  -- TODO: Fix once we add monad transformer
           )

instance MonadGLFWInput GameMonad where
  getGLFWInput = GameMonad get
  putGLFWInput = GameMonad . put

-- The game timer has two parts. The first is the time after the last rendering
-- and the second is the amount of time left over from performing the
-- simulation steps.
type GameTime = (UTCTime, NominalDiffTime)

-- | A `GameWire` is a wire that has all of the features of a standard netwire
-- value. In particular, it can choose to inhibit (not produce a value) and
-- each frame it executes an action within the GameMonad.
type GameWire = W.Wire TimeStep String GameMonad

-- | A `ResourceContextWire` is a wire that has an associated rendering
-- resource under its purview.
type ResourceContext s = ReaderT s GameMonad
newtype ResourceContextWire s a b =
  RCW { getResourceWire :: W.Wire TimeStep String (ResourceContext s) a b }
  deriving ( Functor
           , Applicative
           , Alternative
           , Floating
           , Fractional
           , Monoid
           , Num
           , Choice
           , Profunctor
           , Strong
           , Category
           , Arrow
           , ArrowZero
           , ArrowPlus
           , ArrowChoice
           , ArrowLoop
           )

-- | Continuous wires always produce a value and never inhibit.
newtype ContWire a b =
  CW { getContinuousWire :: GameWire a b }
  deriving ( Functor
           , Applicative
           , Floating
           , Fractional
           , Monoid
           , Num
           , Choice
           , Profunctor
           , Strong
           , Category
           , Arrow
           , ArrowChoice
           , ArrowLoop
           )
