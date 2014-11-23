{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
module Lambency.Shader.Base where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.RWS.Strict

import Linear
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Shader variables

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
} deriving (Show, Eq, Ord)

newtype ShaderVar a = ShaderVar ShaderVarRep deriving (Show, Eq, Ord)
newtype ShaderVarTy a = ShaderVarTy ShaderVarTyRep deriving (Show, Read, Eq, Ord, Bounded)

------------------------------------------------------------
-- Shader Expressions


data UnaryInfix = Negate
                deriving(Show, Eq, Ord, Enum, Bounded)

data UnaryFun = Floor
              | Ceiling
              | Fract
              | Sine
              | Cosine
              | Normalize
              | Length
              | CastFloat
              deriving(Show, Eq, Ord, Enum, Bounded)

data UnaryOp = UnaryInfixOp UnaryInfix
             | UnaryFunOp UnaryFun
              deriving(Show, Eq, Ord)

data BinaryInfix = Add
                 | Sub
                 | Mult
                 | Div
                 | GreaterThan
                 | LessThan
                 deriving(Show, Eq, Ord, Enum, Bounded)

data BinaryFunction = Max
                    | Min
                    | Dot
                    | Pow
                    | Sample1D
                    | Sample2D
                    | Sample3D
                    deriving(Show, Eq, Ord, Enum, Bounded)

data BinaryOp = BinaryInfixOp BinaryInfix
              | BinaryFunOp BinaryFunction
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
               deriving (Eq, Show)

data SwizzleVar = SwizzleX | SwizzleY | SwizzleZ | SwizzleW
                deriving(Show, Eq, Ord, Enum, Bounded)

data ExprRep = VarExpr ShaderVarRep
             | ConstExpr Constant
             | SwizzleExpr ExprRep (SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar)
             | Unary UnaryOp ExprRep
             | Binary BinaryOp ExprRep ExprRep
             | Ternary TernaryOp ExprRep ExprRep ExprRep
             | NewVec VecExpr
               deriving (Eq, Show)

newtype Expr a = Expr ExprRep deriving (Eq, Show)

------------------------------------------------------------
-- Compiled shader statements

data DeclarationTy = AttributeDeclTy
                   | UniformDeclTy
                   | VaryingDeclTy
                   | ConstDeclTy
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Declaration = Attribute ShaderVarRep
                 | Uniform ShaderVarRep
                 | Varying ShaderVarRep
                 | ConstDecl ShaderVarRep ExprRep
                 deriving (Eq, Show)

data SpecialVar = VertexPosition
                | FragmentColor
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Statement = LocalDecl ShaderVarRep (Maybe ExprRep)
               | Assignment ShaderVarRep ExprRep
               | SpecialAssignment SpecialVar ShaderVarRep
               | IfThenElse ExprRep [Statement] [Statement]

newtype ShaderInput i = ShaderInput { getInputVars :: [ShaderVarRep] }

data ShaderOutputVar = CustomOutput String ShaderVarRep
                     | SpecialOutput SpecialVar ShaderVarRep

newtype ShaderOutput o = ShaderOutput { getOutputVars :: [ShaderOutputVar] }

data ShaderType
  = VertexShaderTy
  | FragmentShaderTy
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype ShaderContext i a =
  ShdrCtx { compileShdrCode :: RWST
                               (ShaderInput i, ShaderType)  -- Reader
                               ([Declaration], [Statement]) -- Writer
                               Int                          -- State (varID)
                               Maybe a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus,
            MonadReader (ShaderInput i, ShaderType),
            MonadWriter ([Declaration], [Statement]),
            MonadState Int)

newtype ShaderCode i o = ShdrCode (ShaderContext i (ShaderOutput o))

data ShaderProgram = ShaderProgram {
  shaderDecls :: [Declaration],
  shaderStmts :: [Statement]
}
