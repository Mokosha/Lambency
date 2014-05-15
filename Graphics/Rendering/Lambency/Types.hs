module Graphics.Rendering.Lambency.Types (
  Camera(..), CameraType(..), CameraViewDistance(..),
  LightType(..), Light(..), Shadow(..),
  Shader(..), ShaderVarTy(..), ShaderValue(..), ShaderVar(..), ShaderMap,
  Texture(..), TextureFormat(..), FBOHandle, TextureHandle,
  Material,
  RenderObject(..),
  OutputAction(..),
  Timestep, TimeValue,
  GameWire, GameMonad, GameState, GameSession, GameTime,
  Game(..)
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Graphics.UI.Lambency.Input
import Graphics.UI.Lambency.Sound

import qualified Graphics.Rendering.Lambency.Transform as XForm

import Data.Vect.Float
import Data.Time.Clock

import qualified Data.Map as Map

import qualified Control.Wire as W
import Control.Monad.RWS.Strict

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

data ShaderVarTy = Matrix3Ty
                 | Matrix4Ty
                 | Matrix3ListTy
                 | Matrix4ListTy
                 | Vector3Ty
                 | Vector4Ty
                 | Vector3ListTy
                 | Vector4ListTy
                 | IntTy
                 | IntListTy
                 | FloatTy
                 | FloatListTy
                 | TextureTy GLRaw.GLuint
                 deriving (Show, Eq, Ord)

data ShaderVar = Uniform ShaderVarTy GL.UniformLocation
               | Attribute ShaderVarTy GL.AttribLocation
               deriving (Show, Eq, Ord)

type ShaderVarMap = Map.Map String ShaderVar

data ShaderValue = Matrix3Val Mat3
                 | Matrix4Val Mat4
                 | Matrix3ListVal [Mat3]
                 | Matrix4ListVal [Mat4]
                 | Vector3Val Vec3
                 | Vector4Val Vec4
                 | Vector3ListVal [Vec3]
                 | Vector4ListVal [Vec4]
                 | IntVal Int
                 | IntListVal [Int]
                 | FloatVal Float
                 | FloatListVal [Float]
                 | TextureVal Texture
                 deriving (Show)

type ShaderMap = Map.Map String ShaderValue

data Shader = Shader GL.Program ShaderVarMap deriving(Show, Eq)

--------------------------------------------------------------------------------

-- Textures

type FBOHandle = GL.FramebufferObject
type TextureHandle = GL.TextureObject
data TextureFormat = RGBA8 | RGB8
                     deriving(Show, Eq)

data Texture = Texture TextureHandle TextureFormat
             | RenderTexture TextureHandle FBOHandle
               deriving(Show, Eq)

--------------------------------------------------------------------------------

-- Lights

data LightType = SpotLight Vec3 Normal3 Float
               | DirectionalLight Normal3
               | PointLight Vec3
               | NoLight
               deriving (Show)

data Shadow = Shadow Shader Texture deriving (Show, Eq)
data Light = Light Shader ShaderMap (Maybe Shadow)
           deriving (Show)

--------------------------------------------------------------------------------

-- Materials

-- Material consists of the variables specified by the engine for the shader.
type Material = ShaderMap

--------------------------------------------------------------------------------

-- Renderable Objects

data RenderObject = RenderObject {
  material :: Material,
  render :: Shader -> ShaderMap -> IO ()
}

type RenderInstance = (XForm.Transform, RenderObject)

--------------------------------------------------------------------------------

-- Output functions
--
-- These functions are used to create side effects from our game wires. In
-- general, the most common action will likely be to render something, however
-- we may also want to output sound or a debug string as well.
data OutputAction = LogAction String
                  | SoundAction Sound SoundCommand
                  | Render3DAction XForm.Transform RenderObject

--------------------------------------------------------------------------------

-- !FIXME! Game state should be a list of configuration parameters like screen
-- size so that we can do raycasting from mouse coordinates and maybe some
-- other things...
type GameState = ()

-- Game
data Game a = Game {
  staticLights :: [Light],
  staticGeometry :: [RenderInstance],
  mainCamera :: GameWire () Camera,
  dynamicLights :: [GameWire () Light],
  gameLogic :: GameWire a a
  }

-- !FIXME! Timestep shouldn't need to be a Num, but since
-- Netwire requires HasTime t s for t to be a Real, it should
-- be a Num too....
type Timestep = Float
type TimeValue = W.Timed Timestep ()

--------------------------------------------------------------------------------

-- Game State

type GameMonad = RWS GameState [OutputAction] Input
type GameWire a b = W.Wire TimeValue () GameMonad a b
type GameSession = W.Session IO TimeValue

-- The game timer has two parts. The first is the time after the last rendering
-- and the second is the amount of time left over from performing the
-- simulation steps.
type GameTime = (UTCTime, NominalDiffTime)
