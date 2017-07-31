module Lambency.Types (
  Vec2f, Vec3f, Vec4f, Quatf, Mat2f, Mat3f, Mat4f,
  Camera(..), CameraType(..), CameraViewDistance(..),
  LightVar(..), LightParams(..), LightType(..), Light(..),
  ShadowMap(..), ShadowTechnique(..),
  Shader(..), ShaderVarTy(..), ShaderValue(..), ShaderVar(..), ShaderMap,
  Texture(..), TextureSize(..), TextureFormat(..), FBOHandle, TextureHandle(..),
  MaterialVar(..), NormalModulation(..), ReflectionInfo(..), Material(..),
  RenderFlag(..), RenderObject(..), RenderAction(..), RenderActions(..),
  OutputAction(..),
  TimeStep,
  GameConfig(..), GameWire, GameMonad, GameState, GameSession, GameTime,
  Game(..)
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as GLRaw

import Lambency.Sound

import qualified Lambency.Transform as XForm

import Data.Maybe (isJust)
import Data.Hashable
import Data.Time.Clock

import qualified Data.Map as Map

import qualified Control.Wire as W
import Control.Monad.RWS.Strict

import FRP.Netwire.Input.GLFW

import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import qualified Linear.Quaternion as Quat

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

data ShaderValue = Matrix2Val (Mat2f)
                 | Matrix3Val (Mat3f)
                 | Matrix4Val (Mat4f)
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
  | TexturedSpriteMaterial {
    spriteTextureMatrix :: MaterialVar (M33 Float),
    spriteTexture :: MaterialVar Texture,
    spriteAlpha :: MaterialVar Float
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

data RenderObject = RenderObject {
  material :: Material,
  render :: Shader -> ShaderMap -> IO (),
  flags :: [RenderFlag]
}

data RenderAction = RenderObjects [RenderObject]
                  | RenderClipped RenderAction RenderAction
                  | RenderTransformed XForm.Transform RenderAction
                  | RenderCons RenderAction RenderAction

data RenderActions = RenderActions {
  renderScene :: RenderAction,
  renderUI :: RenderAction
}

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
  windowSize :: (Int, Int)   -- Size of the rendering window in pixels
  }

type GameState = RenderActions

-- Game
data Game a = Game {
  mainCamera :: GameWire () Camera,
  dynamicLights :: [GameWire () Light],
  gameLogic :: GameWire a a
  }

--------------------------------------------------------------------------------

-- Game State

type TimeStep = W.Timed Float ()
type GameMonad = GLFWInputT (RWS GameConfig [OutputAction] GameState)
type GameWire = W.Wire TimeStep String GameMonad
type GameSession = W.Session IO TimeStep

-- The game timer has two parts. The first is the time after the last rendering
-- and the second is the amount of time left over from performing the
-- simulation steps.
type GameTime = (UTCTime, NominalDiffTime)
