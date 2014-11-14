module Lambency.Vertex (
  Vertex2,
  Vertex3,
  TVertex3,
  OVertex3,
  OTVertex3,

  getVertex2Position,
  getVertex3Position,
  getTexVertex3Position,
  getNormVertex3Position,
  getNormTexVertex3Position,

  addNormalV3,
  addNormalTV3,

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

data Vertex2 = Vertex2 !Vec2f deriving (Show, Read, Eq, Ord)
data Vertex3 = Vertex3 !Vec3f deriving (Show, Read, Eq, Ord)
data TVertex3 = TVertex3 !Vec3f !Vec2f deriving (Show, Read, Eq, Ord)
data OVertex3 = OVertex3 !Vec3f !Vec3f deriving (Show, Read, Eq, Ord)
data OTVertex3 = OTVertex3 !Vec3f !Vec3f !Vec2f deriving (Show, Read, Eq, Ord)

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

class (Eq a, Ord a, Storable a) => Vertex a where
  getOpenGLDescriptors :: a -> [GL.VertexArrayDescriptor Float]
  getAttribNames :: a -> [String]

instance Vertex Vertex2 where
  getOpenGLDescriptors (Vertex2 _) = [
    GL.VertexArrayDescriptor 2 GL.Float 0 (nullPtr :: Ptr Float)]
  getAttribNames (Vertex2 _) = ["position"]

instance Vertex Vertex3 where
  getOpenGLDescriptors (Vertex3 _) = [
    GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)]
  getAttribNames (Vertex3 _) = ["position"]

instance Vertex TVertex3 where
  getOpenGLDescriptors (TVertex3 _ _) = [
    GL.VertexArrayDescriptor 3 GL.Float 20 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 2 GL.Float 20 (plusPtr (nullPtr :: Ptr Float) 12)]
  getAttribNames (TVertex3 _ _) = ["position", "texCoord"]

instance Vertex OVertex3 where
  getOpenGLDescriptors (OVertex3 _ _) = [
    GL.VertexArrayDescriptor 3 GL.Float 24 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 3 GL.Float 24 (plusPtr (nullPtr :: Ptr Float) 12)]
  getAttribNames (OVertex3 _ _) = ["position", "normal"]

instance Vertex OTVertex3 where
  getOpenGLDescriptors (OTVertex3 _ _ _) = [
    GL.VertexArrayDescriptor 3 GL.Float 32 (nullPtr :: Ptr Float),
    GL.VertexArrayDescriptor 3 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 12),
    GL.VertexArrayDescriptor 2 GL.Float 32 (plusPtr (nullPtr :: Ptr Float) 24)]
  getAttribNames (OTVertex3 _ _ _) = ["position", "normal", "texCoord"]

class HasTextureCoordinates a where
  getTextureCoordinates :: a -> Vec2f

instance HasTextureCoordinates TVertex3 where
  getTextureCoordinates (TVertex3 _ uv) = uv

instance HasTextureCoordinates OTVertex3 where
  getTextureCoordinates (OTVertex3 _ _ uv) = uv

addNormalV3 :: Vertex3 -> Vec3f -> OVertex3
addNormalV3 (Vertex3 x) = OVertex3 x

addNormalTV3 :: TVertex3 -> Vec3f -> OTVertex3
addNormalTV3 (TVertex3 x uv) n = OTVertex3 x n uv

getVertex3Position :: Vertex3 -> Vec3f
getVertex3Position (Vertex3 x) = x

getVertex2Position :: Vertex2 -> Vec2f
getVertex2Position (Vertex2 x) = x

getTexVertex3Position :: TVertex3 -> Vec3f
getTexVertex3Position (TVertex3 x _) = x

getNormVertex3Position :: OVertex3 -> Vec3f
getNormVertex3Position (OVertex3 x _) = x

getNormTexVertex3Position :: OTVertex3 -> Vec3f
getNormTexVertex3Position (OTVertex3 x _ _) = x

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
