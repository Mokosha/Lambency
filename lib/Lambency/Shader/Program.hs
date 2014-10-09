{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lambency.Shader.Program where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.RWS.Strict

import Lambency.Shader.Var
import Lambency.Shader.Expr
--------------------------------------------------------------------------------

data Statement = Declaration ShaderVarRep ExprRep
               | Assignment ShaderVarRep ExprRep
               | IfThenElse (Expr Bool) Statement Statement

newtype ShaderContext u a = ShdrCtx { compileShdrCode :: RWS u [Statement] Int a }
                            deriving (Functor, Monad)

instance Applicative (ShaderContext u) where
  pure = return
  (ShdrCtx ff) <*> (ShdrCtx xf) = ShdrCtx . RWST $ \y s -> do
    (f, id1, s1) <- runRWST ff y s
    (x, id2, s2) <- runRWST xf y id1
    return (f x, id2, s1 ++ s2)
