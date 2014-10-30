{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Shader.Program where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.RWS.Strict

import qualified Data.Map as Map

import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Vertex
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

newtype ShaderInput i = ShaderInput [ShaderVarRep]
newtype ShaderOutput o = ShaderOutput [ShaderVarRep]

newtype ShaderContext i a = ShdrCtx { compileShdrCode :: RWS (ShaderInput i) ([Declaration], [Statement]) Int a }
                          deriving (Functor, Monad, MonadWriter ([Declaration], [Statement]), MonadState Int)

newtype ShaderCode i o = ShdrCode (ShaderContext i (ShaderOutput o))

instance Applicative (ShaderContext i) where
  pure = return
  (ShdrCtx ff) <*> (ShdrCtx xf) = ShdrCtx . RWST $ \y s -> do
    (f, id1, (d1, s1)) <- runRWST ff y s
    (x, id2, (d2, s2)) <- runRWST xf y id1
    return (f x, id2, (d1 ++ d2, s1 ++ s2))

newVar :: String -> ShaderVarTy a -> ShaderContext i (ShaderVar a)
newVar name ty = do
  varID <- get
  let nextVarID = varID + 1
      var = ShdrVarRep name nextVarID ty
  put nextVarID
  return var

newUniformVar :: String -> ShaderVarTy a -> ShaderContext i (ShaderVar a)
newUniformVar n t = do
  var <- newVar n t
  tell ([Uniform var], mempty)
  return var

data ShaderProgram = ShaderProgram {
  shaderUniforms :: Map.Map String ShaderVarRep,
  shaderDecls :: [Declaration],
  shaderStmts :: [Statement]
}

emptyPrg :: ShaderProgram
emptyPrg = ShaderProgram Map.empty [] []

data Shader v = Shader {
  vertexProgram :: ShaderProgram,
  fragmentProgram :: ShaderProgram
}

mkAttributes :: forall v. Vertex v => VertexTy v -> ShaderInput v
mkAttributes _ =
  let attribNames = getAttribNames (undefined :: v)
      descriptors = getOpenGLDescriptors (undefined :: v)

      getDescriptorTy (GL.VertexArrayDescriptor 3 GL.Float _ _) = Vector3Ty
      getDescriptorTy (GL.VertexArrayDescriptor 2 GL.Float _ _) = Vector2Ty
      getDescriptorTy _ = error "Not implemented!"

      mkVar a n d = ShdrVarRep a n (getDescriptorTy d)
  in
   ShaderInput $ zipWith3 mkVar attribNames [0,1..] descriptors

compileProgram :: VertexTy v -> ShaderCode v o -> ShaderCode o f -> Shader v
compileProgram inputs vertexPrg fragmentPrg = Shader emptyPrg emptyPrg
