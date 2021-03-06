module Lambency.Vertex (
  Vertex2,
  Vertex3,
  TVertex3,
  OVertex3,
  OTVertex3,

  VertexTy,

  getVertex2Position,
  getVertex3Position,
  getTexVertex3Position,
  getNormVertex3Position,
  getNormTexVertex3Position,

  addNormalV3,
  addNormalTV3,

  addTexCoordV3,
  addTexCoordOV3,

  Vertex(..),

  VertexAttributeTy(..),
  VertexAttribute(..),

  HasTextureCoordinates(..),

  mkVertex3,
  mkVertex2,
  mkTexVertex3,
  mkNormVertex3,
  mkNormTexVertex3,
) where

--------------------------------------------------------------------------------
import Foreign.Storable

import Linear.V2
import Linear.V3

import Foreign.Ptr
--------------------------------------------------------------------------------

type Vec2f = V2 Float
type Vec3f = V3 Float

data VertexAttributeTy = FloatAttribTy
                       | IntAttribTy
                       | DoubleAttribTy
                       deriving (Enum, Bounded, Read, Show, Eq, Ord)

data VertexAttribute = VertexAttribute Int VertexAttributeTy

data Vertex2 = Vertex2 !Vec2f deriving (Show, Read, Eq, Ord)
data Vertex3 = Vertex3 !Vec3f deriving (Show, Read, Eq, Ord)
data TVertex3 = TVertex3 !Vec3f !Vec2f deriving (Show, Read, Eq, Ord)
data OVertex3 = OVertex3 !Vec3f !Vec3f deriving (Show, Read, Eq, Ord)
data OTVertex3 = OTVertex3 !Vec3f !Vec3f !Vec2f deriving (Show, Read, Eq, Ord)

data VertexTyRep = Vertex2Ty
                 | Vertex3Ty
                 | TVertex3Ty
                 | OVertex3Ty
                 | OTVertex3Ty
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

newtype VertexTy a = VertexTy VertexTyRep

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

class (Show a, Eq a, Ord a, Storable a) => Vertex a where
  getVertexTy :: a -> VertexTy a
  getVertexAttributes :: a -> [VertexAttribute]
  getAttribNames :: a -> [String]

instance Vertex Vertex2 where
  getVertexTy _ = VertexTy Vertex2Ty
  getVertexAttributes _ = [
    VertexAttribute 2 FloatAttribTy]
  getAttribNames _ = ["position"]

instance Vertex Vertex3 where
  getVertexTy _ = VertexTy Vertex3Ty
  getVertexAttributes _ = [
    VertexAttribute 3 FloatAttribTy]
  getAttribNames _ = ["position"]

instance Vertex TVertex3 where
  getVertexTy _ = VertexTy TVertex3Ty
  getVertexAttributes _ = [
    VertexAttribute 3 FloatAttribTy,
    VertexAttribute 2 FloatAttribTy]
  getAttribNames _ = ["position", "texCoord"]

instance Vertex OVertex3 where
  getVertexTy _ = VertexTy OVertex3Ty
  getVertexAttributes _ = [
    VertexAttribute 3 FloatAttribTy,
    VertexAttribute 3 FloatAttribTy]
  getAttribNames _ = ["position", "normal"]

instance Vertex OTVertex3 where
  getVertexTy _ = VertexTy OTVertex3Ty
  getVertexAttributes _ = [
    VertexAttribute 3 FloatAttribTy,
    VertexAttribute 3 FloatAttribTy,
    VertexAttribute 2 FloatAttribTy]
  getAttribNames _ = ["position", "normal", "texCoord"]

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

addTexCoordV3 :: Vertex3 -> Vec2f -> TVertex3
addTexCoordV3 (Vertex3 x) = TVertex3 x

addTexCoordOV3 :: OVertex3 -> Vec2f -> OTVertex3
addTexCoordOV3 (OVertex3 x n) uv = OTVertex3 x n uv

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
