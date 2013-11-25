module Graphics.Rendering.Lambency.Vertex (
  Vertex,
  getDescriptors,
  getAttribNames,
  isTextured,
  toFloats,

  mkVertex3,
  mkVertex2,
  mkTexVertex3,
  mkNormTexVertex3,
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Data.Vect.Float

import Foreign.Ptr
--------------------------------------------------------------------------------

data Vertex = Vertex2 Vec2
            | Vertex3 Vec3
            | TVertex3 Vec3 Vec2
            | OTVertex3 Vec3 Vec3 Vec2

getDescriptors :: Vertex -> [GL.VertexArrayDescriptor Float]
getDescriptors (TVertex3 _ _) = [
  GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float),
  GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)
  ]
getDescriptors (OTVertex3 _ _ _) = [
  GL.VertexArrayDescriptor 3 GL.Float 32 (nullPtr :: Ptr Float),
  GL.VertexArrayDescriptor 3 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 12),
  GL.VertexArrayDescriptor 2 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 24)
  ]
getDescriptors (Vertex2 _) = [GL.VertexArrayDescriptor 2 GL.Float 0 (nullPtr :: Ptr Float)]
getDescriptors (Vertex3 _) = [GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)]

getAttribNames :: Vertex -> [String]
getAttribNames (TVertex3 _ _) = ["position", "texCoord"]
getAttribNames (OTVertex3 _ _ _) = ["position", "normal", "texCoord"]
getAttribNames (Vertex2 _) = ["position"]
getAttribNames (Vertex3 _) = ["position"]

toFloats :: Vertex -> [Float]
toFloats (Vertex2 v) = [_1 v, _2 v]
toFloats (Vertex3 v) = [_1 v, _2 v, _3 v]
toFloats (TVertex3 p uv) = [_1 p, _2 p, _3 p, _1 uv, _2 uv]
toFloats (OTVertex3 p n uv) = [_1 p, _2 p, _3 p, _1 n, _2 n, _3 n, _1 uv, _2 uv]

isTextured :: Vertex -> Bool
isTextured (TVertex3 _ _) = True
isTextured (OTVertex3 _ _ _) = True
isTextured _ = False

mkVertex3 :: Vec3 -> Vertex
mkVertex3 = Vertex3

mkVertex2 :: Vec2 -> Vertex
mkVertex2 = Vertex2

mkTexVertex3 :: Vec3 -> Vec2 -> Vertex
mkTexVertex3 = TVertex3

mkNormTexVertex3 :: Vec3 -> Vec3 -> Vec2 -> Vertex
mkNormTexVertex3 = OTVertex3
