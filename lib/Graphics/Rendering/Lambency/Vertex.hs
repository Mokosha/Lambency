module Graphics.Rendering.Lambency.Vertex (
  Vertex,
  getDescriptors,
  getAttribNames,
  isTextured,
  toFloats,

  mkVertex3,
  mkVertex2,
  mkTexVertex3,
  mkNormVertex3,
  mkNormTexVertex3,
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Linear.V2
import Linear.V3

import Foreign.Ptr
--------------------------------------------------------------------------------

type Vec2f = V2 Float
type Vec3f = V3 Float

data Vertex = Vertex2 !Vec2f
            | Vertex3 !Vec3f
            | TVertex3 !Vec3f !Vec2f
            | OVertex3 !Vec3f !Vec3f
            | OTVertex3 !Vec3f !Vec3f !Vec2f
            deriving (Show)

getDescriptors :: Vertex -> [GL.VertexArrayDescriptor Float]
getDescriptors (TVertex3 _ _) = [
  GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float),
  GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)
  ]
getDescriptors (OVertex3 _ _) = [
  GL.VertexArrayDescriptor 3 GL.Float 24 (nullPtr :: Ptr Float),
  GL.VertexArrayDescriptor 3 GL.Float 24 (plusPtr (nullPtr :: Ptr Float) 12)
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
getAttribNames (OVertex3 _ _) = ["position", "normal"]
getAttribNames (OTVertex3 _ _ _) = ["position", "normal", "texCoord"]
getAttribNames (Vertex2 _) = ["position"]
getAttribNames (Vertex3 _) = ["position"]

-- !FIXME! How do lenses work? These are hacked in
-- place of the _x _y and _z functions provided by
-- Linear
class Functor f => Index2 f where
  _1 :: f a -> a
  _2 :: f a -> a

class Index2 f => Index3 f where
  _3 :: f a -> a

instance Index2 V2 where
  _1 (V2 x _) = x
  _2 (V2 _ y) = y

instance Index2 V3 where
  _1 (V3 x _ _) = x
  _2 (V3 _ y _) = y

instance Index3 V3 where
  _3 (V3 _ _ z) = z

toFloats :: Vertex -> [Float]
toFloats (Vertex2 v) = [_1 v, _2 v]
toFloats (Vertex3 v) = [_1 v, _2 v, _3 v]
toFloats (TVertex3 p uv) = [_1 p, _2 p, _3 p, _1 uv, _2 uv]
toFloats (OVertex3 p n) = [_1 p, _2 p, _3 p, _1 n, _2 n, _3 n]
toFloats (OTVertex3 p n uv) = [_1 p, _2 p, _3 p, _1 n, _2 n, _3 n, _1 uv, _2 uv]

isTextured :: Vertex -> Bool
isTextured (TVertex3 _ _) = True
isTextured (OTVertex3 _ _ _) = True
isTextured _ = False

mkVertex3 :: Vec3f -> Vertex
mkVertex3 = Vertex3

mkVertex2 :: Vec2f -> Vertex
mkVertex2 = Vertex2

mkTexVertex3 :: Vec3f -> Vec2f -> Vertex
mkTexVertex3 = TVertex3

mkNormVertex3 :: Vec3f -> Vec3f -> Vertex
mkNormVertex3 = OVertex3

mkNormTexVertex3 :: Vec3f -> Vec3f -> Vec2f -> Vertex
mkNormTexVertex3 = OTVertex3
