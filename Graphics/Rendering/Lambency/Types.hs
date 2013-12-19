module Graphics.Rendering.Lambency.Types (
  Camera(..), CameraType(..), CameraViewDistance(..),
  LightType(..), Light(..), Shadow(..),
  Shader(..), ShaderVarTy(..), ShaderValue(..), ShaderVar(..), ShaderMap,
  Texture(..), TextureFormat(..), FBOHandle, TextureHandle,
  Material,
  RenderObject(..),
  Timestep, GameWire
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw

import Graphics.UI.Lambency.Input

import qualified Graphics.Rendering.Lambency.Transform as XForm

import Data.Vect.Float

import qualified Data.Map as Map

import qualified Control.Wire as W

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
  deriving (Show, Eq)

data Camera = Camera XForm.Transform CameraType CameraViewDistance deriving(Show, Eq)

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
               deriving (Show)

data Shadow = Shadow Shader Texture deriving (Show, Eq)
data Light = Light Shader ShaderMap (Maybe Shadow)
           deriving (Show)

--------------------------------------------------------------------------------

-- Materials

-- Material consists of the variables specified by the
-- engine for the shader. If the material has a render texture associated with
-- it, then a MultiMaterial allows the specification of a default material for
-- all objects to use during the off-screen rendering pass of the material
type Material = ShaderMap

--------------------------------------------------------------------------------

-- Renderable Objects

data RenderObject = RenderObject {
  material :: Material,
  render :: Shader -> ShaderMap -> IO ()
}
{--
--------------------------------------------------------------------------------

-- Game Objects

data GameObject a =
  Object {
    location :: XForm.Transform,
    renderObject :: Maybe RenderObject,
    object :: a,
    update :: GameWire a
  }

newtype GameObjectID = GameObjectID { gameObjectID :: Int } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------

-- Game State

data FixedState = FixedState {
  textures :: Map.Map TextureID Texture,
  materials :: Map.Map MaterialID Material
} deriving (Show)

data DynamicState a = DynamicState {
  mainCamera :: (CameraWire a),
  lights :: Map.Map LightID (LightWire a),
  objects :: Map.Map GameObjectID (GameObject a)
}

type RenderCall = IO ()
data RenderPass = NoRender
                | Render Camera [RenderCall] RenderPass

--------------------------------------------------------------------------------

-- Manipulators

type GameContext a = RWS.RWS FixedState [String] (DynamicState a)
type Timestep = () -> W.Timed Float ()

type GameWire a = W.Wire
                  Timestep
                  Bool
                  (GameContext a)
                  Input
                  (Input, GameObject a)

type LightWire a = W.Wire
                   Timestep
                   Bool
                   (GameContext a)
                   Input
                   (Input, Light)

type CameraWire a = W.Wire
                    Timestep
                    Bool
                    (GameContext a)
                    Input
                    (Input, Camera)

type GameSession a = W.Session (GameContext a) Timestep
--}

type Timestep = () -> W.Timed Float ()
type GameWire = W.Wire Timestep Bool IO Input (Input, [Light], [RenderObject])
                
