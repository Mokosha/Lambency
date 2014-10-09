module Lambency.Shader.Expr where

--------------------------------------------------------------------------------
import Lambency.Shader.Var
--------------------------------------------------------------------------------

data UnaryOp = Negate
             deriving(Show, Eq, Ord, Enum)

data BinaryOp = Add
              | Sub
              | Mult
              | Div
              deriving(Show, Eq, Ord, Enum)

data TernaryOp = Clamp
               | Mix
               deriving(Show, Eq, Ord, Enum)

data ExprRep = VarExpr ShaderVarRep
             | Unary UnaryOp ExprRep
             | Binary BinaryOp ExprRep ExprRep
             | Ternary TernaryOp ExprRep ExprRep ExprRep

type Expr a = ExprRep
