{-# LANGUAGE EmptyDataDecls #-}
module Lambency.Shader.Var where

--------------------------------------------------------------------------------
import Linear
--------------------------------------------------------------------------------

data Sampler1D
data Sampler2D
data Sampler3D

data ShaderVarTyRep = Matrix2Ty
                    | Matrix3Ty
                    | Matrix4Ty
                    | Matrix3ListTy
                    | Matrix4ListTy
                    | Vector2Ty
                    | Vector3Ty
                    | Vector4Ty
                    | Vector2ListTy
                    | Vector3ListTy
                    | Vector4ListTy
                    | IntTy
                    | IntListTy
                    | FloatTy
                    | FloatListTy
                    | Sampler1DTy
                    | Sampler2DTy
                    | Sampler3DTy
                    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data ShaderVarRep = ShdrVarRep {
  shdrVarName :: String,
  shdrVarID :: Int,
  shdrVarTy :: ShaderVarTyRep
} deriving (Show, Eq)

newtype ShaderVar a = ShaderVar ShaderVarRep
newtype ShaderVarTy a = ShaderVarTy ShaderVarTyRep deriving (Show, Read, Eq, Ord, Bounded)

matrix2Ty :: ShaderVarTy (M22 Float)
matrix2Ty = ShaderVarTy Matrix2Ty

matrix3Ty :: ShaderVarTy (M33 Float)
matrix3Ty = ShaderVarTy Matrix3Ty

matrix4Ty :: ShaderVarTy (M44 Float)
matrix4Ty = ShaderVarTy Matrix4Ty

vector2fTy :: ShaderVarTy (V2 Float)
vector2fTy = ShaderVarTy Vector2Ty

vector3fTy :: ShaderVarTy (V3 Float)
vector3fTy = ShaderVarTy Vector3Ty

vector4fTy :: ShaderVarTy (V4 Float)
vector4fTy = ShaderVarTy Vector4Ty

vector2iTy :: ShaderVarTy (V2 Int)
vector2iTy = ShaderVarTy Vector2Ty

vector3iTy :: ShaderVarTy (V3 Int)
vector3iTy = ShaderVarTy Vector3Ty

vector4iTy :: ShaderVarTy (V4 Int)
vector4iTy = ShaderVarTy Vector4Ty

intTy :: ShaderVarTy Int
intTy = ShaderVarTy IntTy

floatTy :: ShaderVarTy Float
floatTy = ShaderVarTy FloatTy

sampler1DTy :: ShaderVarTy Sampler1D
sampler1DTy = ShaderVarTy Sampler1DTy

sampler2DTy :: ShaderVarTy Sampler2D
sampler2DTy = ShaderVarTy Sampler2DTy

sampler3DTy :: ShaderVarTy Sampler3D
sampler3DTy = ShaderVarTy Sampler3DTy
