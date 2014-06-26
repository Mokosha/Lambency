module Lambency.Vertex (
  Vertex2,
  Vertex3,
  TVertex3,
  OVertex3,
  OTVertex3,

  Vertex(..),
  HasTextureCoordinates(..),

  mkVertex3,
  mkVertex2,
  mkTexVertex3,
  mkNormVertex3,
  mkNormTexVertex3,
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Foreign.Storable

import Linear.V2
import Linear.V3

import Foreign.Ptr
--------------------------------------------------------------------------------

type Vec2f = V2 Float
type Vec3f = V3 Float

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

data Vertex2 = Vertex2 !Vec2f deriving (Show, Read)
data Vertex3 = Vertex3 !Vec3f deriving (Show, Read)
data TVertex3 = TVertex3 !Vec3f !Vec2f deriving (Show, Read)
data OVertex3 = OVertex3 !Vec3f !Vec3f deriving (Show, Read)
data OTVertex3 = OTVertex3 !Vec3f !Vec3f !Vec2f deriving (Show, Read)

instance Storable Vertex2 where
  sizeOf (Vertex2 p) = sizeOf p
  alignment (Vertex2 p) = alignment p
  peekElemOff ptr off = peekElemOff (castPtr ptr) off >>= (return . Vertex2)
  pokeElemOff ptr off (Vertex2 p) = pokeElemOff (castPtr ptr) off p

instance Storable Vertex3 where
  sizeOf (Vertex3 p) = sizeOf p
  alignment (Vertex3 p) = alignment p
  peekElemOff ptr off = peekElemOff (castPtr ptr) off >>= (return . Vertex3)
  pokeElemOff ptr off (Vertex3 p) = pokeElemOff (castPtr ptr) off p

instance Storable TVertex3 where
  sizeOf (TVertex3 p uv) = (sizeOf p) + (sizeOf uv)
  alignment (TVertex3 p uv) = max (alignment p) (alignment uv)

  peek ptr = let
    fptr = (castPtr :: Ptr TVertex3 -> Ptr Float) ptr
    in do
      p <- peek (castPtr ptr)
      uv <- peek (castPtr (fptr `plusPtr` 3))
      return $ TVertex3 p uv

  poke ptr (TVertex3 p uv) = let
    fptr = (castPtr :: Ptr TVertex3 -> Ptr Float) ptr
    in do
      poke (castPtr ptr) p
      poke (castPtr (fptr `plusPtr` 3)) uv

instance Storable OVertex3 where
  sizeOf (OVertex3 p n) = (sizeOf p) + (sizeOf n)
  alignment (OVertex3 p n) = max (alignment p) (alignment n)
  peekElemOff ptr off = do
    p <- peekElemOff (castPtr ptr) (2*off)
    n <- peekElemOff (castPtr ptr) (2*off + 1)
    return $ OVertex3 p n

  pokeElemOff ptr off (OVertex3 p n) = do
    pokeElemOff (castPtr ptr) (2*off) p
    pokeElemOff (castPtr ptr) (2*off + 1) n

instance Storable OTVertex3 where
  sizeOf (OTVertex3 p n uv) = (sizeOf p) + (sizeOf n) + (sizeOf uv)
  -- !KLUDGE! n and p are the same type, so their alignment is
  -- identical. Ideally we should do a max3 here
  alignment (OTVertex3 p _ uv) = max (alignment p) (alignment uv)

  peek ptr = let
    fptr = (castPtr :: Ptr OTVertex3 -> Ptr Float) ptr
    in do
      p <- peek (castPtr ptr)
      n <- peek (castPtr (fptr `plusPtr` 3))
      uv <- peek (castPtr (fptr `plusPtr` 6))
      return $ OTVertex3 p n uv

  poke ptr (OTVertex3 p n uv) = let
    fptr = (castPtr :: Ptr OTVertex3 -> Ptr Float) ptr
    in do
      poke (castPtr ptr) p
      poke (castPtr (fptr `plusPtr` 3)) n
      poke (castPtr (fptr `plusPtr` 6)) uv

class Storable a => Vertex a where
  getOpenGLDescriptors :: a -> [GL.VertexArrayDescriptor Float]
  getAttribNames :: a -> [String]
  toFloats :: a -> [Float]

instance Vertex Vertex2 where
  getOpenGLDescriptors (Vertex2 _) = [
    GL.VertexArrayDescriptor 2 GL.Float 0 (nullPtr :: Ptr Float)]
  getAttribNames (Vertex2 _) = ["position"]
  toFloats (Vertex2 v) = [_1 v, _2 v]

instance Vertex Vertex3 where
  getOpenGLDescriptors (Vertex3 _) = [
    GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)]
  getAttribNames (Vertex3 _) = ["position"]
  toFloats (Vertex3 v) = [_1 v, _2 v, _3 v]

instance Vertex TVertex3 where
  getOpenGLDescriptors (TVertex3 _ _) = [
    GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)]
  getAttribNames (TVertex3 _ _) = ["position", "texCoord"]
  toFloats (TVertex3 p uv) = [_1 p, _2 p, _3 p, _1 uv, _2 uv]

instance Vertex OVertex3 where
  getOpenGLDescriptors (OVertex3 _ _) = [
    GL.VertexArrayDescriptor 3 GL.Float 24 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 3 GL.Float 24 (plusPtr (nullPtr :: Ptr Float) 12)]
  getAttribNames (OVertex3 _ _) = ["position", "normal"]
  toFloats (OVertex3 p n) = [_1 p, _2 p, _3 p, _1 n, _2 n, _3 n]

instance Vertex OTVertex3 where
  getOpenGLDescriptors (OTVertex3 _ _ _) = [
    GL.VertexArrayDescriptor 3 GL.Float 32 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 3 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 12),
    GL.VertexArrayDescriptor 2 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 24)]
  getAttribNames (OTVertex3 _ _ _) = ["position", "normal", "texCoord"]
  toFloats (OTVertex3 p n uv) = [_1 p, _2 p, _3 p, _1 n, _2 n, _3 n, _1 uv, _2 uv]

class HasTextureCoordinates a where
  getTextureCoordinates :: a -> Vec2f

instance HasTextureCoordinates TVertex3 where
  getTextureCoordinates (TVertex3 _ uv) = uv

instance HasTextureCoordinates OTVertex3 where
  getTextureCoordinates (OTVertex3 _ _ uv) = uv

mkVertex3 :: Vec3f -> Vertex3
mkVertex3 = Vertex3

mkVertex2 :: Vec2f -> Vertex2
mkVertex2 = Vertex2

mkTexVertex3 :: Vec3f -> Vec2f -> TVertex3
mkTexVertex3 = TVertex3

mkNormVertex3 :: Vec3f -> Vec3f -> OVertex3
mkNormVertex3 = OVertex3

mkNormTexVertex3 :: Vec3f -> Vec3f -> Vec2f -> OTVertex3
mkNormTexVertex3 = OTVertex3
