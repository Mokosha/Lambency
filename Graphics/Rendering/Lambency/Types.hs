module Graphics.Rendering.Lambency.Types (
  Camera(..), CameraType(..), CameraViewDistance(..),
  LightType(..), Light(..), Shadow(..),
  Shader(..), ShaderVarTy(..), ShaderValue(..), ShaderVar(..), ShaderMap,
  Texture(..), TextureFormat(..), FBOHandle, TextureHandle,
  Material,
  RenderObject(..),
  Component(..),
  LogAction(..),
  GameWire, Timestep, GameMonad, GameState,
  GameObject(..), Game
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Graphics.UI.Lambency.Input

import qualified Graphics.Rendering.Lambency.Transform as XForm
import qualified Graphics.Rendering.Lambency.Bounds as BV

import Data.Vect.Float

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

--------------------------------------------------------------------------------

-- Components
-- 
-- A component is a way to annotate a game object with certain properties. For
-- example, a component may be a collider or a render object. Only game objects
-- that have a render object will be rendered. A game object will only be able
-- to register collision with other objects that have a collider, etc.
data Component = CollisionComponent BV.BoundingVolume
               | RenderComponent RenderObject

--------------------------------------------------------------------------------

-- Logging functions
--
data LogAction = StringOutput String

--------------------------------------------------------------------------------

-- Game Objects

type Timestep = () -> W.Timed Float ()
type GameMonad = RWS GameState [LogAction] Input
type GameWire a = W.Wire Timestep () GameMonad () a

-- GameObjects are a set of components and the way to update the components
-- based on user input. The output of the wire associated with a game object is
-- all of the new objects that will be added to the game environment.
data GameObject = GameObject XForm.Transform [Component]

-- A GameState is a camera with a collection of game objects
type GameState = (Camera, [Light], [GameObject])
type Game = (GameWire Camera, [GameWire Light], [GameWire [GameObject]])
