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

data SpecialVar = VertexPosition
                | FragmentColor
                deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Statement = LocalDecl ShaderVarRep (Maybe ExprRep)
               | Assignment ShaderVarRep ExprRep
               | SpecialAssignment SpecialVar ShaderVarRep
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

setE :: ShaderVarTy a -> Expr a -> ShaderContext i (ShaderVar a)
setE ty e = do
  v <- newVar "_t" ty
  tell (mempty, [LocalDecl v (Just e)])
  return v

assignE :: ShaderVar a -> Expr a -> ShaderContext i ()
assignE v e = tell (mempty, [Assignment v e])

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

compileProgram :: Vertex v => VertexTy v -> ShaderCode v o -> ShaderCode o f -> Shader v
compileProgram iptTy (ShdrCode vertexPrg) (ShdrCode fragmentPrg) =
  let vs_input = mkAttributes iptTy
      (ShaderOutput (ShaderIO vs_output), varID, (vs_decls, vs_stmts)) =
        runRWS (compileShdrCode vertexPrg) vs_input 0
      fs_input_vars = tail vs_output
      fs_input = ShaderInput $ ShaderIO fs_input_vars
      (ShaderOutput (ShaderIO fs_output), _, (fs_decls, fs_stmts)) =
        runRWS (compileShdrCode fragmentPrg) fs_input 0

      varyingDecls = map Varying fs_input_vars
  in
   Shader {
     vertexProgram = ShaderProgram {
        shaderDecls = vs_decls ++ varyingDecls,
        shaderStmts = vs_stmts ++ [SpecialAssignment VertexPosition $ head vs_output]
        },
     fragmentProgram = ShaderProgram {
       shaderDecls = fs_decls ++ varyingDecls,
       shaderStmts = fs_stmts ++ [SpecialAssignment FragmentColor $ head fs_output]
       }
     }
    
