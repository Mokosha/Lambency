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
                    deriving (Show, Eq, Ord, Enum)

data ShaderVarRep = ShdrVarRep {
  shdrVarName :: String,
  shdrVarID :: Int,
  shdrVarTy :: ShaderVarTyRep
}

type ShaderVar a = ShaderVarRep
type ShaderVarTy a = ShaderVarTyRep

matrix2Ty :: ShaderVarTy (M22 Float)
matrix2Ty = Matrix2Ty

matrix3Ty :: ShaderVarTy (M33 Float)
matrix3Ty = Matrix3Ty

matrix4Ty :: ShaderVarTy (M44 Float)
matrix4Ty = Matrix4Ty

vector2fTy :: ShaderVarTy (V2 Float)
vector2fTy = Vector2Ty

vector3fTy :: ShaderVarTy (V2 Float)
vector3fTy = Vector3Ty

vector4fTy :: ShaderVarTy (V2 Float)
vector4fTy = Vector4Ty

vector2iTy :: ShaderVarTy (V2 Int)
vector2iTy = Vector2Ty

vector3iTy :: ShaderVarTy (V2 Int)
vector3iTy = Vector3Ty

vector4iTy :: ShaderVarTy (V2 Int)
vector4iTy = Vector4Ty

intTy :: ShaderVarTy Int
intTy = IntTy

floatTy :: ShaderVarTy Float
floatTy = FloatTy

sampler1DTy :: ShaderVarTy Sampler1D
sampler1DTy = Sampler1DTy

sampler2DTy :: ShaderVarTy Sampler2D
sampler2DTy = Sampler2DTy

sampler3DTy :: ShaderVarTy Sampler3D
sampler3DTy = Sampler3DTy
