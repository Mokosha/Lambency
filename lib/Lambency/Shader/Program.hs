{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Shader.Program where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.RWS.Strict

import Lambency.Vertex
import Lambency.Shader.Var
import Lambency.Shader.Expr

import Linear
--------------------------------------------------------------------------------

data DeclarationTy = AttributeDeclTy
                   | UniformDeclTy
                   | VaryingDeclTy
                   | ConstDeclTy
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Declaration = Attribute ShaderVarRep
                 | Uniform ShaderVarRep
                 | Varying ShaderVarRep
                 | ConstDecl ShaderVarRep ExprRep

getDeclType :: Declaration -> DeclarationTy
getDeclType (Attribute _) = AttributeDeclTy
getDeclType (Uniform _) = UniformDeclTy
getDeclType (Varying _) = VaryingDeclTy
getDeclType (ConstDecl _ _) = ConstDeclTy

data Statement = LocalDecl ShaderVarRep (Maybe ExprRep)
               | Assignment ShaderVarRep ExprRep
               | IfThenElse (Expr Bool) [Statement] [Statement]

newtype ShaderIO = ShaderIO [ShaderVarRep]

newtype ShaderInput i = ShaderInput ShaderIO
newtype ShaderOutput o = ShaderOutput ShaderIO
newtype ShaderContext i a =
  ShdrCtx { compileShdrCode :: RWS (ShaderInput i) ([Declaration], [Statement]) Int a }
  deriving (Functor, Applicative, Monad, MonadReader (ShaderInput i),
            MonadWriter ([Declaration], [Statement]), MonadState Int)

newtype ShaderCode i o = ShdrCode (ShaderContext i (ShaderOutput o))

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
  shaderDecls :: [Declaration],
  shaderStmts :: [Statement]
}

emptyPrg :: ShaderProgram
emptyPrg = ShaderProgram [] []

data Shader v = Shader {
  vertexProgram :: ShaderProgram,
  fragmentProgram :: ShaderProgram
}

attribToVarTy :: VertexAttribute -> ShaderVarTyRep
attribToVarTy (VertexAttribute 1 IntAttribTy) = IntTy
attribToVarTy (VertexAttribute 1 FloatAttribTy) = FloatTy
attribToVarTy (VertexAttribute 2 FloatAttribTy) = Vector2Ty
attribToVarTy (VertexAttribute 3 FloatAttribTy) = Vector3Ty
attribToVarTy (VertexAttribute 4 FloatAttribTy) = Vector4Ty
attribToVarTy _ = error "Not implemented!"

mkAttributes :: forall v. Vertex v => VertexTy v -> ShaderInput v
mkAttributes _ =
  let attribs = getVertexAttributes (undefined :: v)
      mkVar n = ShdrVarRep ("attrib" ++ (show n)) n . attribToVarTy
  in
   ShaderInput $ ShaderIO (zipWith mkVar [0,1..] attribs)

getInput :: ShaderVarTyRep -> Int -> ShaderContext i (ShaderVarRep)
getInput expected idx = do
  (ShaderInput (ShaderIO vars)) <- ask
  let v@(ShdrVarRep _ _ ty) = vars !! idx
  if expected == ty
    then return v
    else error $ concat ["Type mismatch for attribute ", show idx,
                         ": Expected ", show expected, " got ", show ty]

getInputi :: Int -> ShaderContext i (ShaderVar Int)
getInputi = getInput IntTy

getInputf :: Int -> ShaderContext i (ShaderVar Float)
getInputf = getInput FloatTy

getInput2f :: Int -> ShaderContext i (ShaderVar (V2 Float))
getInput2f = getInput Vector2Ty

getInput3f :: Int -> ShaderContext i (ShaderVar (V3 Float))
getInput3f = getInput Vector3Ty

getInput4f :: Int -> ShaderContext i (ShaderVar (V4 Float))
getInput4f = getInput Vector4Ty

compileProgram :: VertexTy v -> ShaderCode v o -> ShaderCode o f -> Shader v
compileProgram inputs vertexPrg fragmentPrg = Shader emptyPrg emptyPrg
