module Lambency.Vertex (
  Vertex2,
  Vertex3,
  TVertex3,
  OVertex3,
  OTVertex3,

  VertexTy,

  vertex2Ty,
  vertex3Ty,
  texVertex3Ty,
  normVertex3Ty,
  normTexVertex3Ty,

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

data Vertex2 = Vertex2 !Vec2f deriving (Show, Read)
data Vertex3 = Vertex3 !Vec3f deriving (Show, Read)
data TVertex3 = TVertex3 !Vec3f !Vec2f deriving (Show, Read)
data OVertex3 = OVertex3 !Vec3f !Vec3f deriving (Show, Read)
data OTVertex3 = OTVertex3 !Vec3f !Vec3f !Vec2f deriving (Show, Read)

data VertexTyRep = Vertex2Ty
                 | Vertex3Ty
                 | TVertex3Ty
                 | OVertex3Ty
                 | OTVertex3Ty
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

type VertexTy a = VertexTyRep

vertex2Ty :: VertexTy Vertex2
vertex2Ty = Vertex2Ty

vertex3Ty :: VertexTy Vertex3
vertex3Ty = Vertex3Ty

texVertex3Ty :: VertexTy TVertex3
texVertex3Ty = TVertex3Ty

normVertex3Ty :: VertexTy OVertex3
normVertex3Ty = OVertex3Ty

normTexVertex3Ty :: VertexTy OTVertex3
normTexVertex3Ty = OTVertex3Ty

instance Storable Vertex2 where
  sizeOf _ = sizeOf (undefined :: Vec2f)
  alignment _ = alignment (undefined :: Vec2f)
  peekElemOff ptr off = peekElemOff (castPtr ptr) off >>= (return . Vertex2)
  pokeElemOff ptr off (Vertex2 p) = pokeElemOff (castPtr ptr) off p

instance Storable Vertex3 where
  sizeOf _ = sizeOf (undefined :: Vec3f)
  alignment _ = alignment (undefined :: Vec3f)
  peekElemOff ptr off = peekElemOff (castPtr ptr) off >>= (return . Vertex3)
  pokeElemOff ptr off (Vertex3 p) = pokeElemOff (castPtr ptr) off p

instance Storable TVertex3 where
  sizeOf _ = (sizeOf (undefined :: Vec3f)) +
             (sizeOf (undefined :: Vec2f))
  alignment _ = max (alignment (undefined :: Vec3f)) (alignment (undefined :: Vec2f))

  peek ptr = do
    p <- peek (castPtr ptr)
    uv <- peek (castPtr (ptr `plusPtr` 12))
    return $ TVertex3 p uv

  poke ptr (TVertex3 p uv) = do
    poke (castPtr ptr) p
    poke (castPtr (ptr `plusPtr` 12)) uv

instance Storable OVertex3 where
  sizeOf _ = (sizeOf (undefined :: Vec3f)) +
             (sizeOf (undefined :: Vec3f))
  alignment _ = alignment (undefined :: Vec3f)

  peek ptr = do
    p <- peek (castPtr ptr)
    n <- peek (castPtr (ptr `plusPtr` 12))
    return $ OVertex3 p n

  poke ptr (OVertex3 p n) = do
    poke (castPtr ptr) p
    poke (castPtr (ptr `plusPtr` 12)) n

instance Storable OTVertex3 where
  sizeOf _ = (sizeOf (undefined :: Vec3f)) +
             (sizeOf (undefined :: Vec3f)) +
             (sizeOf (undefined :: Vec2f))
  alignment _ = max (alignment (undefined :: Vec3f)) (alignment (undefined :: Vec2f))

  peek ptr = do
    p <- peek (castPtr ptr)
    n <- peek (castPtr (ptr `plusPtr` 12))
    uv <- peek (castPtr (ptr `plusPtr` 24))
    return $ OTVertex3 p n uv

  poke ptr (OTVertex3 p n uv) = do
    poke (castPtr ptr) p
    poke (castPtr (ptr `plusPtr` 12)) n
    poke (castPtr (ptr `plusPtr` 24)) uv

class Storable a => Vertex a where
  getOpenGLDescriptors :: a -> [GL.VertexArrayDescriptor Float]
  getAttribNames :: a -> [String]

instance Vertex Vertex2 where
  getOpenGLDescriptors _ = [
    GL.VertexArrayDescriptor 2 GL.Float 0 (nullPtr :: Ptr Float)]
  getAttribNames _ = ["position"]

instance Vertex Vertex3 where
  getOpenGLDescriptors _ = [
    GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)]
  getAttribNames _ = ["position"]

instance Vertex TVertex3 where
  getOpenGLDescriptors _ = [
    GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)]
  getAttribNames _ = ["position", "texCoord"]

instance Vertex OVertex3 where
  getOpenGLDescriptors _ = [
    GL.VertexArrayDescriptor 3 GL.Float 24 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 3 GL.Float 24 (plusPtr (nullPtr :: Ptr Float) 12)]
  getAttribNames _ = ["position", "normal"]

instance Vertex OTVertex3 where
  getOpenGLDescriptors _ = [
    GL.VertexArrayDescriptor 3 GL.Float 32 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 3 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 12),
    GL.VertexArrayDescriptor 2 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 24)]
  getAttribNames _ = ["position", "normal", "texCoord"]

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
