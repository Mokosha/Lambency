{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Shader.Program where

--------------------------------------------------------------------------------
import Control.Monad.RWS.Strict

import Data.List ((\\), intersect)

import Lambency.Vertex

import Lambency.Shader.Base

import Linear
--------------------------------------------------------------------------------

getDeclType :: Declaration -> DeclarationTy
getDeclType (Attribute _) = AttributeDeclTy
getDeclType (Uniform _) = UniformDeclTy
getDeclType (Varying _) = VaryingDeclTy
getDeclType (ConstDecl _ _) = ConstDeclTy

addCustomOVar :: ShaderVar a -> ShaderOutput b -> ShaderOutput b
addCustomOVar (ShaderVar v) (ShaderOutput vs) = ShaderOutput ((CustomOutput v):vs)

addVertexPosition :: ShaderVar (V4 Float) -> ShaderOutput b -> ShaderOutput b
addVertexPosition (ShaderVar v) (ShaderOutput vs) =
  ShaderOutput ((SpecialOutput VertexPosition v) : vs)

addFragmentColor :: ShaderVar (V4 Float) -> ShaderOutput b -> ShaderOutput b
addFragmentColor (ShaderVar v) (ShaderOutput vs) =
  ShaderOutput ((SpecialOutput FragmentColor v) : vs)

emptyO :: ShaderOutput a
emptyO = ShaderOutput []

getOutputVar :: ShaderOutputVar -> ShaderVarRep
getOutputVar (CustomOutput v) = v
getOutputVar (SpecialOutput _ v) = v

collectOutput :: (ShaderOutputVar -> Bool) -> ShaderOutput a -> [ShaderVarRep]
collectOutput fn = map getOutputVar . filter fn . getOutputVars

collectCustom :: ShaderOutput a -> [ShaderVarRep]
collectCustom = collectOutput isCustom
  where
    isCustom (CustomOutput _) = True
    isCustom _ = False

collectSpecial :: ShaderOutput a -> [ShaderVarRep]
collectSpecial = collectOutput isSpecial
  where
    isSpecial (SpecialOutput _ _) = True
    isSpecial _ = False

mkSpecialStmts :: ShaderOutput a -> [Statement]
mkSpecialStmts (ShaderOutput ovars) = concatMap mkStmt ovars
  where
    mkStmt :: ShaderOutputVar -> [Statement]
    mkStmt (SpecialOutput sv v) = [SpecialAssignment sv v]
    mkStmt _ = []

updateStmt :: ShaderVarRep -> Statement -> [Statement]
updateStmt v1 s@(LocalDecl v2 Nothing)
  | v1 == v2 = []
  | otherwise = [s]
updateStmt v1 s@(LocalDecl v2 (Just e))
  | v1 == v2 = [Assignment v1 e]
  | otherwise = [s]
updateStmt _ s@(Assignment _ _) = [s]
updateStmt _ s@(SpecialAssignment _ _) = [s]
updateStmt v (IfThenElse e s1 s2) =
  let output = ShaderOutput [CustomOutput v]
  in [IfThenElse e (updateStmts s1 output) (updateStmts s2 output)]

updateStmts :: [Statement] -> ShaderOutput a -> [Statement]
updateStmts stmts vars =
  let updateFor :: ShaderVarRep -> [Statement] -> [Statement]
      updateFor v = concat . map (updateStmt v)
  in foldl (flip updateFor) stmts (collectCustom vars) ++ (mkSpecialStmts vars)

newVar :: String -> ShaderVarTy a -> ShaderContext i (ShaderVar a)
newVar name (ShaderVarTy ty) = do
  varID <- get
  let nextVarID = varID + 1
      var = ShaderVar $ ShdrVarRep name nextVarID ty
  put nextVarID
  return var

newUniformVar :: String -> ShaderVarTy a -> ShaderContext i (ShaderVar a)
newUniformVar n t = do
  v@(ShaderVar vrep) <- newVar n t
  tell ([Uniform vrep], mempty)
  return v

setE :: ShaderVarTy a -> Expr a -> ShaderContext i (ShaderVar a)
setE ty (Expr e) = do
  v@(ShaderVar vrep) <- newVar "_t" ty
  tell (mempty, [LocalDecl vrep (Just e)])
  return v

assignE :: ShaderVar a -> Expr a -> ShaderContext i ()
assignE (ShaderVar v) (Expr e) = tell (mempty, [Assignment v e])

emptyPrg :: ShaderProgram
emptyPrg = ShaderProgram [] []

data Shader v = Shader {
  vertexProgram :: ShaderProgram,
  fragmentProgram :: ShaderProgram
}

ifThen :: Expr Bool -> ShaderContext i () -> ShaderContext i () -> ShaderContext i ()
ifThen (Expr e) c1 c2 = ShdrCtx $ RWST $ \ipt st ->
  let (_, id1, (decls1, s1)) = runRWS (compileShdrCode c1) ipt st
      (_, id2, (decls2, s2)) = runRWS (compileShdrCode c2) ipt st
  in return ((), id1 + id2, (decls1 ++ decls2, [IfThenElse e s1 s2]))

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
      names = getAttribNames (undefined :: v)
      mkVar (n, i) = ShdrVarRep n i . attribToVarTy
  in
   ShaderInput $ zipWith mkVar (zip names [0,1..]) attribs

getInput :: ShaderVarTy a -> Int -> ShaderContext i (ShaderVar a)
getInput (ShaderVarTy expected) idx = do
  (ShaderInput vars) <- ask
  let v@(ShdrVarRep _ _ ty) = vars !! idx
  if expected == ty
    then return (ShaderVar v)
    else error $ concat ["Type mismatch for attribute ", show idx,
                         ": Expected ", show expected, " got ", show ty]

getInputi :: Int -> ShaderContext i (ShaderVar Int)
getInputi = getInput (ShaderVarTy IntTy)

getInputf :: Int -> ShaderContext i (ShaderVar Float)
getInputf = getInput (ShaderVarTy FloatTy)

getInput2f :: Int -> ShaderContext i (ShaderVar (V2 Float))
getInput2f = getInput (ShaderVarTy Vector2Ty)

getInput3f :: Int -> ShaderContext i (ShaderVar (V3 Float))
getInput3f = getInput (ShaderVarTy Vector3Ty)

getInput4f :: Int -> ShaderContext i (ShaderVar (V4 Float))
getInput4f = getInput (ShaderVarTy Vector4Ty)

copyShdrVars :: Int -> [ShaderVarRep] -> [ShaderVarRep]
copyShdrVars _ [] = []
copyShdrVars lastID ((ShdrVarRep n _ ty) : vs) = (ShdrVarRep n lastID ty) : (copyShdrVars (lastID + 1) vs)

addVertexOutputs :: [ShaderVarRep] -> [ShaderVarRep] -> [Statement]
addVertexOutputs = zipWith setCopyStmt
  where
    setCopyStmt :: ShaderVarRep -> ShaderVarRep -> Statement
    setCopyStmt new old = Assignment new (VarExpr old)

compileProgram :: Vertex v => VertexTy v -> ShaderCode v o -> ShaderCode o f -> Shader v
compileProgram iptTy (ShdrCode vertexPrg) (ShdrCode fragmentPrg) =
  let vs_input@(ShaderInput vs_input_vars) = mkAttributes iptTy
      (vs_output, varID, (vs_decls, vs_stmts')) =
        runRWS (compileShdrCode vertexPrg) vs_input (length vs_input_vars)

      vs_output_vars = collectCustom vs_output
      (fs_input_vars, vs_stmts) =
        case vs_output_vars `intersect` vs_input_vars of
          [] -> (vs_output_vars, updateStmts vs_stmts' vs_output)
          bothIO -> 
            let newVars = copyShdrVars (varID + 1) bothIO
            in
             ((vs_output_vars \\ bothIO) ++ newVars, 
              updateStmts vs_stmts' vs_output ++ (addVertexOutputs newVars bothIO))

      fs_input = ShaderInput fs_input_vars
      (fs_output, _, (fs_decls, fs_stmts)) =
        runRWS (compileShdrCode fragmentPrg) fs_input (varID + length fs_input_vars)

      varyingDecls = map Varying fs_input_vars
      attribDecls = map Attribute vs_input_vars
  in
   Shader {
     vertexProgram =
       ShaderProgram {
         shaderDecls = concat [attribDecls, vs_decls, varyingDecls],
         shaderStmts = vs_stmts
         },
     fragmentProgram =
       ShaderProgram {
         shaderDecls = fs_decls ++ varyingDecls,
         shaderStmts = updateStmts fs_stmts fs_output
       }
     }
    
