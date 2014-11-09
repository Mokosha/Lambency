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
                    | Sample1D
                    | Sample2D
                    | Sample3D
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

data VecExpr = Vec2Expr ExprRep ExprRep
             | Vec3Expr ExprRep ExprRep ExprRep
             | Vec4Expr ExprRep ExprRep ExprRep ExprRep

data ExprRep = VarExpr ShaderVarRep
             | ConstExpr Constant
             | SwizzleExpr ExprRep (SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar)
             | Unary UnaryOp ExprRep
             | Binary BinaryOp ExprRep ExprRep
             | Ternary TernaryOp ExprRep ExprRep ExprRep
             | NewVec VecExpr

newtype Expr a = Expr ExprRep

mkConstMat2 :: M22 Float -> Expr (M22 Float)
mkConstMat2 m = Expr $ ConstExpr $ ConstMat2 m

mkConstMat3 :: M33 Float -> Expr (M33 Float)
mkConstMat3 m = Expr $ ConstExpr $ ConstMat3 m

mkConstMat4 :: M44 Float -> Expr (M44 Float)
mkConstMat4 m = Expr $ ConstExpr $ ConstMat4 m

mkConstVec2f :: V2 Float -> Expr (V2 Float)
mkConstVec2f v = Expr $ ConstExpr $ ConstVec2f v

mkConstVec3f :: V3 Float -> Expr (V3 Float)
mkConstVec3f v = Expr $ ConstExpr $ ConstVec3f v

mkConstVec4f :: V4 Float -> Expr (V4 Float)
mkConstVec4f v = Expr $ ConstExpr $ ConstVec4f v

mkConstVec2i :: V2 Int -> Expr (V2 Int)
mkConstVec2i v = Expr $ ConstExpr $ ConstVec2i v

mkConstVec3i :: V3 Int -> Expr (V3 Int)
mkConstVec3i v = Expr $ ConstExpr $ ConstVec3i v

mkConstVec4i :: V4 Int -> Expr (V4 Int)
mkConstVec4i v = Expr $ ConstExpr $ ConstVec4i v

mkConstf :: Float -> Expr Float
mkConstf f = Expr $ ConstExpr $ ConstFloat f

mkConsti :: Int -> Expr Int
mkConsti i = Expr $ ConstExpr $ ConstInt i

mkVarExpr :: ShaderVar a -> Expr a
mkVarExpr (ShaderVar v) = Expr $ VarExpr v

--------------------------------------------------

binaryExpr :: BinaryOp -> Expr a -> Expr b -> Expr c
binaryExpr op (Expr e1) (Expr e2) = Expr $ Binary op e1 e2

xform3f :: Expr (M33 Float) -> Expr (V3 Float) -> Expr (V3 Float)
xform3f = binaryExpr (BinaryInfixOp Mult)

xform4f :: Expr (M44 Float) -> Expr (V4 Float) -> Expr (V4 Float)
xform4f = binaryExpr (BinaryInfixOp Mult)

multf :: Expr Float -> Expr Float -> Expr Float
multf = binaryExpr (BinaryInfixOp Mult)

div3f :: Expr (V3 Float) -> Expr Float -> Expr (V3 Float)
div3f = binaryExpr (BinaryInfixOp Div)

sample1D :: Expr Sampler1D -> Expr Float -> Expr (V4 Float)
sample1D = binaryExpr (BinaryFunctionOp Sample1D)

sample2D :: Expr Sampler2D -> Expr (V2 Float) -> Expr (V4 Float)
sample2D = binaryExpr (BinaryFunctionOp Sample2D)

sample3D :: Expr Sampler3D -> Expr (V3 Float) -> Expr (V4 Float)
sample3D = binaryExpr (BinaryFunctionOp Sample3D)

--------------------------------------------------------------------------------

mkVec2 :: Expr a -> Expr a -> Expr (V2 a)
mkVec2 (Expr e1) (Expr e2) = Expr $ NewVec $ Vec2Expr e1 e2

mkVec2f :: Expr Float -> Expr Float -> Expr (V2 Float)
mkVec2f = mkVec2

mkVec3 :: Expr a -> Expr a -> Expr a -> Expr (V3 a)
mkVec3 (Expr e1) (Expr e2) (Expr e3) = Expr $ NewVec $ Vec3Expr e1 e2 e3

mkVec3f_111 :: Expr Float -> Expr Float -> Expr Float -> Expr (V3 Float)
mkVec3f_111 = mkVec3

mkVec3f_12 :: Expr Float -> Expr (V2 Float) -> Expr (V3 Float)
mkVec3f_12 x v =
  let sw = swizzle2D v
      f = finishSwizzleS :: Sw2D1D Float -> Expr Float
  in mkVec3 x ((f . _x_) sw) ((f . _y_) sw)

mkVec3f_21 :: Expr (V2 Float) -> Expr Float -> Expr (V3 Float)
mkVec3f_21 v z =
  let sw = swizzle2D v
      f = finishSwizzleS
  in mkVec3 ((f . _x_) sw) ((f . _y_) sw) z

mkVec4 :: Expr a -> Expr a -> Expr a -> Expr a -> Expr (V4 a)
mkVec4 (Expr x) (Expr y) (Expr z) (Expr w) = Expr $ NewVec $ Vec4Expr x y z w

mkVec4f_1111 :: Expr Float -> Expr Float -> Expr Float -> Expr Float -> Expr (V4 Float)
mkVec4f_1111 = mkVec4

mkVec4f_211 :: Expr (V2 Float) -> Expr Float -> Expr Float -> Expr (V4 Float)
mkVec4f_211 v z w =
  let sw = swizzle2D v
      f = finishSwizzleS
  in mkVec4 ((f . _x_) sw) ((f . _y_) sw) z w

mkVec4f_121 :: Expr Float -> Expr (V2 Float) -> Expr Float -> Expr (V4 Float)
mkVec4f_121 x v w =
  let sw = swizzle2D v
      f = finishSwizzleS
  in mkVec4 x ((f . _x_) sw) ((f . _y_) sw) w

mkVec4f_112 :: Expr Float -> Expr Float -> Expr (V2 Float) -> Expr (V4 Float)
mkVec4f_112 x y v =
  let sw = swizzle2D v
      f = finishSwizzleS
  in mkVec4 x y ((f . _x_) sw) ((f . _y_) sw)

mkVec4f_22 :: Expr (V2 Float) -> Expr (V2 Float) -> Expr (V4 Float)
mkVec4f_22 v1 v2 =
  let s = swizzle2D
      f = finishSwizzleS
  in mkVec4 ((f . _x_ . s) v1) ((f . _y_ . s) v1) ((f . _x_ . s) v2) ((f . _y_ . s) v2)

mkVec4f_31 :: Expr (V3 Float) -> Expr Float -> Expr (V4 Float)
mkVec4f_31 v w =
  let s = swizzle3D
      f = finishSwizzleS
  in mkVec4 ((f . _x_ . s) v) ((f . _y_ . s) v) ((f . _z_ . s) v) w

mkVec4f_13 :: Expr Float -> Expr (V3 Float) -> Expr (V4 Float)
mkVec4f_13 x v =
  let s = swizzle3D
      f = finishSwizzleS
  in mkVec4 x ((f . _x_ . s) v) ((f . _y_ . s) v) ((f . _z_ . s) v)

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

data Sw2D1D a = Sw2D1D (Expr (V2 a)) SwizzleVar
data Sw3D1D a = Sw3D1D (Expr (V3 a)) SwizzleVar
data Sw4D1D a = Sw4D1D (Expr (V4 a)) SwizzleVar

data Sw2D2D a = Sw2D2D (Sw2D1D a) SwizzleVar
data Sw3D2D a = Sw3D2D (Sw3D1D a) SwizzleVar
data Sw4D2D a = Sw4D2D (Sw4D1D a) SwizzleVar

data Sw2D3D a = Sw2D3D (Sw2D2D a) SwizzleVar
data Sw3D3D a = Sw3D3D (Sw3D2D a) SwizzleVar
data Sw4D3D a = Sw4D3D (Sw4D2D a) SwizzleVar

data Sw2D4D a = Sw2D4D (Sw2D3D a) SwizzleVar
data Sw3D4D a = Sw3D4D (Sw3D3D a) SwizzleVar
data Sw4D4D a = Sw4D4D (Sw4D3D a) SwizzleVar


swizzle2D :: Expr (V2 a) -> Sw2D a
swizzle2D = Sw2D

swizzle3D :: Expr (V3 a) -> Sw3D a
swizzle3D = Sw3D

swizzle4D :: Expr (V4 a) -> Sw4D a
swizzle4D = Sw4D

class SwizzleExpressibleS a where
  finishSwizzleS :: a t -> Expr t

instance SwizzleExpressibleS Sw2D1D where
  finishSwizzleS (Sw2D1D (Expr e) v) = Expr $ SwizzleExpr e (v, Nothing, Nothing, Nothing)

instance SwizzleExpressibleS Sw3D1D where
  finishSwizzleS (Sw3D1D (Expr e) v) = Expr $ SwizzleExpr e (v, Nothing, Nothing, Nothing)

instance SwizzleExpressibleS Sw4D1D where
  finishSwizzleS (Sw4D1D (Expr e) v) = Expr $ SwizzleExpr e (v, Nothing, Nothing, Nothing)

class SwizzleExpressibleV a b | a -> b where
  finishSwizzleV :: a t -> Expr (b t)

instance SwizzleExpressibleV Sw2D V2 where
  finishSwizzleV (Sw2D expr) = expr

instance SwizzleExpressibleV Sw3D V3 where
  finishSwizzleV (Sw3D expr) = expr

instance SwizzleExpressibleV Sw4D V4 where
  finishSwizzleV (Sw4D expr) = expr

instance SwizzleExpressibleV Sw2D2D V2 where
  finishSwizzleV (Sw2D2D (Sw2D1D (Expr e) v1) v2) = Expr $ SwizzleExpr e (v1, Just v2, Nothing, Nothing)

instance SwizzleExpressibleV Sw3D2D V2 where
  finishSwizzleV (Sw3D2D (Sw3D1D (Expr e) v1) v2) = Expr $ SwizzleExpr e (v1, Just v2, Nothing, Nothing)

instance SwizzleExpressibleV Sw4D2D V2 where
  finishSwizzleV (Sw4D2D (Sw4D1D (Expr e) v1) v2) = Expr $ SwizzleExpr e (v1, Just v2, Nothing, Nothing)

instance SwizzleExpressibleV Sw2D3D V3 where
  finishSwizzleV (Sw2D3D (Sw2D2D (Sw2D1D (Expr e) v1) v2) v3) = Expr $ SwizzleExpr e (v1, Just v2, Just v3, Nothing)

instance SwizzleExpressibleV Sw3D3D V3 where
  finishSwizzleV (Sw3D3D (Sw3D2D (Sw3D1D (Expr e) v1) v2) v3) = Expr $ SwizzleExpr e (v1, Just v2, Just v3, Nothing)

instance SwizzleExpressibleV Sw4D3D V3 where
  finishSwizzleV (Sw4D3D (Sw4D2D (Sw4D1D (Expr e) v1) v2) v3) = Expr $ SwizzleExpr e (v1, Just v2, Just v3, Nothing)

instance SwizzleExpressibleV Sw2D4D V4 where
  finishSwizzleV (Sw2D4D (Sw2D3D (Sw2D2D (Sw2D1D (Expr e) v1) v2) v3) v4) = Expr $ SwizzleExpr e (v1, Just v2, Just v3, Just v4)

instance SwizzleExpressibleV Sw3D4D V4 where
  finishSwizzleV (Sw3D4D (Sw3D3D (Sw3D2D (Sw3D1D (Expr e) v1) v2) v3) v4) = Expr $ SwizzleExpr e (v1, Just v2, Just v3, Just v4)

instance SwizzleExpressibleV Sw4D4D V4 where
  finishSwizzleV (Sw4D4D (Sw4D3D (Sw4D2D (Sw4D1D (Expr e) v1) v2) v3) v4) = Expr $ SwizzleExpr e (v1, Just v2, Just v3, Just v4)

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
