module Lambency.Types (
  Vec2f, Vec3f, Vec4f, Quatf, Mat2f, Mat3f, Mat4f,
  Camera(..), CameraType(..), CameraViewDistance(..),
  LightType(..), Light(..), Shadow(..),
  Shader(..), ShaderVarTy(..), ShaderValue(..), ShaderVar(..), ShaderMap,
  Texture(..), TextureFormat(..), FBOHandle, TextureHandle,
  Material,
  RenderObject(..),
  OutputAction(..),
  TimeStep,
  GameWire, GameMonad, GameState, GameSession, GameTime,
  Game(..)
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Lambency.Input
import Lambency.Sound

import qualified Lambency.Transform as XForm

import Data.Time.Clock

import qualified Data.Map as Map

import qualified Control.Wire as W
import Control.Monad.RWS.Strict

import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import qualified Linear.Quaternion as Quat

--------------------------------------------------------------------------------

-- Vector Types

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

data ShaderValue = Matrix3Val (Mat3f)
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

data LightType = SpotLight Vec3f Vec3f Float
               | DirectionalLight Vec3f
               | PointLight Vec3f
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

--------------------------------------------------------------------------------

-- Game State

type TimeStep = W.Timed Float ()
type GameMonad = RWS GameState [OutputAction] Input
type GameWire a b = W.Wire TimeStep () GameMonad a b
type GameSession = W.Session IO TimeStep

-- The game timer has two parts. The first is the time after the last rendering
-- and the second is the amount of time left over from performing the
-- simulation steps.
type GameTime = (UTCTime, NominalDiffTime)
