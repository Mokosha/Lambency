module Lambency.Shader.Var where

--------------------------------------------------------------------------------
import Lambency.Shader.Base

import Linear
--------------------------------------------------------------------------------

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

shadow2DTy :: ShaderVarTy Shadow2D
shadow2DTy = ShaderVarTy Shadow2DTy
