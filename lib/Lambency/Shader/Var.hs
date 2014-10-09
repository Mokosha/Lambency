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

data Constant = ConstMat2 (M22 Float)
              | ConstMat3 (M33 Float)
              | ConstMat4 (M44 Float)
              | ConstVec2f (V2 Float)
              | ConstVec3f (V3 Float)
              | ConstVec4f (V4 Float)
              | ConstVec2i (V2 Int)
              | ConstVec3i (V3 Int)
              | ConstVec4i (V4 Int)
              | ConstFloat Float
              | ConstInt Int
                deriving (Show, Ord, Eq)

data ShaderVarRep = ShdrVarRep {
  shdrVarName :: String,
  shdrVarID :: Int,
  shdrVarTy :: ShaderVarTyRep,
  shdrVarBinding :: Maybe Constant
}

type ShaderVar a = ShaderVarRep
type ShaderVarTy a = ShaderVarTyRep

mkConstMat2 :: M22 Float -> ShaderVar (M22 Float)
mkConstMat2 m = ShdrVarRep "_t" (-1) Matrix2Ty (Just $ ConstMat2 m)

mkConstMat3 :: M33 Float -> ShaderVar (M33 Float)
mkConstMat3 m = ShdrVarRep "_t" (-1) Matrix3Ty (Just $ ConstMat3 m)

mkConstMat4 :: M44 Float -> ShaderVar (M44 Float)
mkConstMat4 m = ShdrVarRep "_t" (-1) Matrix4Ty (Just $ ConstMat4 m)

mkConstVec2f :: V2 Float -> ShaderVar (V2 Float)
mkConstVec2f v = ShdrVarRep "_t" (-1) Vector2Ty (Just $ ConstVec2f v)

mkConstVec3f :: V3 Float -> ShaderVar (V3 Float)
mkConstVec3f v = ShdrVarRep "_t" (-1) Vector3Ty (Just $ ConstVec3f v)

mkConstVec4f :: V4 Float -> ShaderVar (V4 Float)
mkConstVec4f v = ShdrVarRep "_t" (-1) Vector4Ty (Just $ ConstVec4f v)

mkConstVec2i :: V2 Int -> ShaderVar (V2 Int)
mkConstVec2i v = ShdrVarRep "_t" (-1) Vector2Ty (Just $ ConstVec2i v)

mkConstVec3i :: V3 Int -> ShaderVar (V3 Int)
mkConstVec3i v = ShdrVarRep "_t" (-1) Vector3Ty (Just $ ConstVec3i v)

mkConstVec4i :: V4 Int -> ShaderVar (V4 Int)
mkConstVec4i v = ShdrVarRep "_t" (-1) Vector4Ty (Just $ ConstVec4i v)

mkConstf :: Float -> ShaderVar (Float)
mkConstf f = ShdrVarRep "_t" (-1) Vector4Ty (Just $ ConstFloat f)

mkConsti :: Int -> ShaderVar (Int)
mkConsti i = ShdrVarRep "_t" (-1) Vector2Ty (Just $ ConstInt i)

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
