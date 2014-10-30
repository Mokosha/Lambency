{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lambency.Shader.Program where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.RWS.Strict

import Lambency.Shader.Var
import Lambency.Shader.Expr
--------------------------------------------------------------------------------

data DeclarationTy = AttributeTy
                   | UniformTy
                   | VaryingTy

data Declaration = Attribute ShaderVarRep
                 | Uniform ShaderVarRep
                 | Varying ShaderVarRep
                 | ConstDecl ExprRep

data Statement = LocalDecl ShaderVarRep ExprRep
               | Assignment ShaderVarRep ExprRep
               | IfThenElse (Expr Bool) Statement Statement

newtype ShaderContext u a = ShdrCtx { compileShdrCode :: RWS u [Statement] Int a }
                            deriving (Functor, Monad, MonadWriter [Statement], MonadReader u, MonadState Int)

instance Applicative (ShaderContext u) where
  pure = return
  (ShdrCtx ff) <*> (ShdrCtx xf) = ShdrCtx . RWST $ \y s -> do
    (f, id1, s1) <- runRWST ff y s
    (x, id2, s2) <- runRWST xf y id1
    return (f x, id2, s1 ++ s2)

newVar :: String -> ShaderVarTy a -> ShaderContext u (ShaderVar a)
newVar name ty = do
  varID <- get
  let nextVarID = varID + 1
      var = ShdrVarRep name nextVarID ty
  put nextVarID
  return var

data ShaderProgram u i o = ShdrPrg {
  shaderDecls :: i -> u -> [Declaration],
  buildShader :: i -> ShaderContext u o
}

compileShader :: u -> i -> ShaderProgram u i o -> ([Declaration], [Statement])
compileShader uniforms inputs (ShdrPrg decls body) = ([], [])
