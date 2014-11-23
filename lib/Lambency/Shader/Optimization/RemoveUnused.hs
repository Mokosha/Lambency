module Lambency.Shader.Optimization.RemoveUnused (
  isShaderVarUsed,
) where

--------------------------------------------------------------------------------
import Lambency.Shader.Base
--------------------------------------------------------------------------------

-- !TODO! Eventually optimize by removing unused statements based on what variables
-- apear in the RHS of the statement expressions

isUsedInExpr :: ShaderVarRep -> ExprRep -> Bool
isUsedInExpr v (VarExpr v') = v == v'
isUsedInExpr _ (ConstExpr _) = False
isUsedInExpr v (SwizzleExpr e _) = isUsedInExpr v e
isUsedInExpr v (Unary _ e) = isUsedInExpr v e
isUsedInExpr v (Binary _ e1 e2) = any (isUsedInExpr v) [e1, e2]
isUsedInExpr v (Ternary _ e1 e2 e3) = any (isUsedInExpr v) [e1, e2, e3]
isUsedInExpr v (NewVec (Vec2Expr e1 e2)) = any (isUsedInExpr v) [e1, e2]
isUsedInExpr v (NewVec (Vec3Expr e1 e2 e3)) = any (isUsedInExpr v) [e1, e2, e3]
isUsedInExpr v (NewVec (Vec4Expr e1 e2 e3 e4)) = any (isUsedInExpr v) [e1, e2, e3, e4]

isUsedInStmt :: ShaderVarRep -> Statement -> Bool
isUsedInStmt _ (LocalDecl _ Nothing) = False
isUsedInStmt v (LocalDecl _ (Just e)) = isUsedInExpr v e
isUsedInStmt v (Assignment _ e) = isUsedInExpr v e
isUsedInStmt v (SpecialAssignment _ v') = v == v'
isUsedInStmt v (IfThenElse e s1 s2) =
  isUsedInExpr v e || isShaderVarUsed v s1 || isShaderVarUsed v s2

isShaderVarUsed :: ShaderVarRep -> [Statement] -> Bool
isShaderVarUsed v = any (isUsedInStmt v)
