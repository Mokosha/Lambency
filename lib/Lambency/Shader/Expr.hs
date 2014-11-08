{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Lambency.Shader.Expr where

--------------------------------------------------------------------------------
import Lambency.Shader.Var

import Linear
--------------------------------------------------------------------------------

data UnaryInfix = Negate
                deriving(Show, Eq, Ord, Enum, Bounded)

data UnaryFun = Floor
              | Ceiling
              | Sine
              | Cosine
              deriving(Show, Eq, Ord, Enum, Bounded)

data UnaryOp = UnaryInfixOp UnaryInfix
             | UnaryFunOp UnaryFun
              deriving(Show, Eq, Ord)

data BinaryInfix = Add
                 | Sub
                 | Mult
                 | Div
                 deriving(Show, Eq, Ord, Enum, Bounded)

data BinaryFunction = Max
                    | Min
                    | Dot
                    deriving(Show, Eq, Ord, Enum, Bounded)

data BinaryOp = BinaryInfixOp BinaryInfix
              | BinaryFunctionOp BinaryFunction
              deriving(Show, Eq, Ord)

data TernaryOp = Clamp
               | Mix
               deriving(Show, Eq, Ord, Enum, Bounded)

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

data ExprRep = VarExpr ShaderVarRep
             | ConstExpr Constant
             | SwizzleExpr ExprRep (SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar)
             | Unary UnaryOp ExprRep
             | Binary BinaryOp ExprRep ExprRep
             | Ternary TernaryOp ExprRep ExprRep ExprRep

type Expr a = ExprRep

mkConstMat2 :: M22 Float -> Expr (M22 Float)
mkConstMat2 m = ConstExpr $ ConstMat2 m

mkConstMat3 :: M33 Float -> Expr (M33 Float)
mkConstMat3 m = ConstExpr $ ConstMat3 m

mkConstMat4 :: M44 Float -> Expr (M44 Float)
mkConstMat4 m = ConstExpr $ ConstMat4 m

mkConstVec2f :: V2 Float -> Expr (V2 Float)
mkConstVec2f v = ConstExpr $ ConstVec2f v

mkConstVec3f :: V3 Float -> Expr (V3 Float)
mkConstVec3f v = ConstExpr $ ConstVec3f v

mkConstVec4f :: V4 Float -> Expr (V4 Float)
mkConstVec4f v = ConstExpr $ ConstVec4f v

mkConstVec2i :: V2 Int -> Expr (V2 Int)
mkConstVec2i v = ConstExpr $ ConstVec2i v

mkConstVec3i :: V3 Int -> Expr (V3 Int)
mkConstVec3i v = ConstExpr $ ConstVec3i v

mkConstVec4i :: V4 Int -> Expr (V4 Int)
mkConstVec4i v = ConstExpr $ ConstVec4i v

mkConstf :: Float -> Expr Float
mkConstf f = ConstExpr $ ConstFloat f

mkConsti :: Int -> Expr Int
mkConsti i = ConstExpr $ ConstInt i

mkVarExpr :: ShaderVar a -> Expr a
mkVarExpr = VarExpr

--------------------------------------------------------------------------------

-- Swizzle

-- Converts glsl of the type:
--   vec3 foo; foo.xyz
--   to
--   let foo = Expr (V3 Float)
--    in (finishSwizzleV . _z_ . _y_ . _x_ . startSwizzle) foo

data SwizzleVar = SwizzleX | SwizzleY | SwizzleZ | SwizzleW

data Sw2D a = Sw2D (Expr (V2 a))
data Sw3D a = Sw3D (Expr (V3 a))
data Sw4D a = Sw4D (Expr (V4 a))

data Sw2D1D a = Sw2D1D ExprRep SwizzleVar
data Sw3D1D a = Sw3D1D ExprRep SwizzleVar
data Sw4D1D a = Sw4D1D ExprRep SwizzleVar

data Sw2D2D a = Sw2D2D (Sw2D1D a) SwizzleVar
data Sw3D2D a = Sw3D2D (Sw3D1D a) SwizzleVar
data Sw4D2D a = Sw4D2D (Sw4D1D a) SwizzleVar

data Sw2D3D a = Sw2D3D (Sw2D2D a) SwizzleVar
data Sw3D3D a = Sw3D3D (Sw3D2D a) SwizzleVar
data Sw4D3D a = Sw4D3D (Sw4D2D a) SwizzleVar

data Sw2D4D a = Sw2D4D (Sw2D3D a) SwizzleVar
data Sw3D4D a = Sw3D4D (Sw3D3D a) SwizzleVar
data Sw4D4D a = Sw4D4D (Sw4D3D a) SwizzleVar

class Swizzlable a b | a -> b where
  startSwizzle :: Expr (a t) -> b t

instance Swizzlable V2 Sw2D where
  startSwizzle = Sw2D

instance Swizzlable V3 Sw3D where
  startSwizzle = Sw3D

instance Swizzlable V4 Sw4D where
  startSwizzle = Sw4D

class SwizzleExpressibleS a where
  finishSwizzleS :: a t -> Expr t

instance SwizzleExpressibleS Sw2D1D where
  finishSwizzleS (Sw2D1D e v) = SwizzleExpr e (v, Nothing, Nothing, Nothing)

instance SwizzleExpressibleS Sw3D1D where
  finishSwizzleS (Sw3D1D e v) = SwizzleExpr e (v, Nothing, Nothing, Nothing)

instance SwizzleExpressibleS Sw4D1D where
  finishSwizzleS (Sw4D1D e v) = SwizzleExpr e (v, Nothing, Nothing, Nothing)

class SwizzleExpressibleV a b | a -> b where
  finishSwizzleV :: a t -> Expr (b t)

instance SwizzleExpressibleV Sw2D V2 where
  finishSwizzleV (Sw2D expr) = expr

instance SwizzleExpressibleV Sw3D V3 where
  finishSwizzleV (Sw3D expr) = expr

instance SwizzleExpressibleV Sw4D V4 where
  finishSwizzleV (Sw4D expr) = expr

instance SwizzleExpressibleV Sw2D2D V2 where
  finishSwizzleV (Sw2D2D (Sw2D1D e v1) v2) = SwizzleExpr e (v1, Just v2, Nothing, Nothing)

instance SwizzleExpressibleV Sw3D2D V2 where
  finishSwizzleV (Sw3D2D (Sw3D1D e v1) v2) = SwizzleExpr e (v1, Just v2, Nothing, Nothing)

instance SwizzleExpressibleV Sw4D2D V2 where
  finishSwizzleV (Sw4D2D (Sw4D1D e v1) v2) = SwizzleExpr e (v1, Just v2, Nothing, Nothing)

instance SwizzleExpressibleV Sw2D3D V3 where
  finishSwizzleV (Sw2D3D (Sw2D2D (Sw2D1D e v1) v2) v3) = SwizzleExpr e (v1, Just v2, Just v3, Nothing)

instance SwizzleExpressibleV Sw3D3D V3 where
  finishSwizzleV (Sw3D3D (Sw3D2D (Sw3D1D e v1) v2) v3) = SwizzleExpr e (v1, Just v2, Just v3, Nothing)

instance SwizzleExpressibleV Sw4D3D V3 where
  finishSwizzleV (Sw4D3D (Sw4D2D (Sw4D1D e v1) v2) v3) = SwizzleExpr e (v1, Just v2, Just v3, Nothing)

instance SwizzleExpressibleV Sw2D4D V4 where
  finishSwizzleV (Sw2D4D (Sw2D3D (Sw2D2D (Sw2D1D e v1) v2) v3) v4) = SwizzleExpr e (v1, Just v2, Just v3, Just v4)

instance SwizzleExpressibleV Sw3D4D V4 where
  finishSwizzleV (Sw3D4D (Sw3D3D (Sw3D2D (Sw3D1D e v1) v2) v3) v4) = SwizzleExpr e (v1, Just v2, Just v3, Just v4)

instance SwizzleExpressibleV Sw4D4D V4 where
  finishSwizzleV (Sw4D4D (Sw4D3D (Sw4D2D (Sw4D1D e v1) v2) v3) v4) = SwizzleExpr e (v1, Just v2, Just v3, Just v4)

class Swizzlable2D a b | a -> b where
  _x_ :: a t -> b t
  _y_ :: a t -> b t

instance Swizzlable2D Sw2D Sw2D1D where
  _x_ (Sw2D expr) = Sw2D1D expr SwizzleX
  _y_ (Sw2D expr) = Sw2D1D expr SwizzleY

instance Swizzlable2D Sw3D Sw3D1D where
  _x_ (Sw3D expr) = Sw3D1D expr SwizzleX
  _y_ (Sw3D expr) = Sw3D1D expr SwizzleY

instance Swizzlable2D Sw4D Sw4D1D where
  _x_ (Sw4D expr) = Sw4D1D expr SwizzleX
  _y_ (Sw4D expr) = Sw4D1D expr SwizzleY

instance Swizzlable2D Sw2D1D Sw2D2D where
  _x_ = flip Sw2D2D SwizzleX
  _y_ = flip Sw2D2D SwizzleY

instance Swizzlable2D Sw3D1D Sw3D2D where
  _x_ = flip Sw3D2D SwizzleX
  _y_ = flip Sw3D2D SwizzleY

instance Swizzlable2D Sw4D1D Sw4D2D where
  _x_ = flip Sw4D2D SwizzleX
  _y_ = flip Sw4D2D SwizzleY

instance Swizzlable2D Sw2D2D Sw2D3D where
  _x_ = flip Sw2D3D SwizzleX
  _y_ = flip Sw2D3D SwizzleY

instance Swizzlable2D Sw3D2D Sw3D3D where
  _x_ = flip Sw3D3D SwizzleX
  _y_ = flip Sw3D3D SwizzleY

instance Swizzlable2D Sw4D2D Sw4D3D where
  _x_ = flip Sw4D3D SwizzleX
  _y_ = flip Sw4D3D SwizzleY

instance Swizzlable2D Sw2D3D Sw2D4D where
  _x_ = flip Sw2D4D SwizzleX
  _y_ = flip Sw2D4D SwizzleY

instance Swizzlable2D Sw3D3D Sw3D4D where
  _x_ = flip Sw3D4D SwizzleX
  _y_ = flip Sw3D4D SwizzleY

instance Swizzlable2D Sw4D3D Sw4D4D where
  _x_ = flip Sw4D4D SwizzleX
  _y_ = flip Sw4D4D SwizzleY

class Swizzlable3D a b | a -> b where
  _z_ :: a t -> b t

instance Swizzlable3D Sw3D Sw3D1D where
  _z_ (Sw3D expr) = Sw3D1D expr SwizzleZ

instance Swizzlable3D Sw4D Sw4D1D where
  _z_ (Sw4D expr) = Sw4D1D expr SwizzleZ

instance Swizzlable3D Sw3D1D Sw3D2D where
  _z_ = flip Sw3D2D SwizzleZ

instance Swizzlable3D Sw4D1D Sw4D2D where
  _z_ = flip Sw4D2D SwizzleZ

instance Swizzlable3D Sw3D2D Sw3D3D where
  _z_ = flip Sw3D3D SwizzleZ

instance Swizzlable3D Sw4D2D Sw4D3D where
  _z_ = flip Sw4D3D SwizzleZ

instance Swizzlable3D Sw3D3D Sw3D4D where
  _z_ = flip Sw3D4D SwizzleZ

instance Swizzlable3D Sw4D3D Sw4D4D where
  _z_ = flip Sw4D4D SwizzleZ

class Swizzlable4D a b | a -> b where
  _w_ :: a t -> b t

instance Swizzlable4D Sw4D Sw4D1D where
  _w_ (Sw4D expr) = Sw4D1D expr SwizzleW

instance Swizzlable4D Sw4D1D Sw4D2D where
  _w_ = flip Sw4D2D SwizzleW

instance Swizzlable4D Sw4D2D Sw4D3D where
  _w_ = flip Sw4D3D SwizzleW

instance Swizzlable4D Sw4D3D Sw4D4D where
  _w_ = flip Sw4D4D SwizzleW
