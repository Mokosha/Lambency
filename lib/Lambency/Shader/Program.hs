{-# LANGUAGE ScopedTypeVariables #-}
module Lambency.Shader.Program where

--------------------------------------------------------------------------------
import Control.Monad.RWS.Strict

import qualified Data.Map as Map

import Lambency.Vertex

import Lambency.Shader.Base
import Lambency.Shader.Optimization

import Linear
--------------------------------------------------------------------------------

getDeclType :: Declaration -> DeclarationTy
getDeclType (Attribute _) = AttributeDeclTy
getDeclType (Uniform _) = UniformDeclTy
getDeclType (Varying _) = VaryingDeclTy
getDeclType (ConstDecl _ _) = ConstDeclTy

addCustomOVar :: String -> ShaderVar a -> ShaderOutput b -> ShaderOutput b
addCustomOVar name (ShaderVar v) (ShaderOutput vs) = ShaderOutput ((CustomOutput name v):vs)

addVertexPosition :: ShaderVar (V4 Float) -> ShaderOutput b -> ShaderOutput b
addVertexPosition (ShaderVar v) (ShaderOutput vs) =
  ShaderOutput ((SpecialOutput VertexPosition v) : vs)

addFragmentColor :: ShaderVar (V4 Float) -> ShaderOutput b -> ShaderOutput b
addFragmentColor (ShaderVar v) (ShaderOutput vs) =
  ShaderOutput ((SpecialOutput FragmentColor v) : vs)

emptyO :: ShaderOutput a
emptyO = ShaderOutput []

varyingPrefix :: String
varyingPrefix = "_varying_"

getOutputVar :: ShaderOutputVar -> ShaderVarRep
getOutputVar (CustomOutput "" v) = v
getOutputVar (CustomOutput name (ShdrVarRep _ i ty)) = ShdrVarRep (varyingPrefix ++ name) i ty
getOutputVar (SpecialOutput _ v) = v

collectOutput :: (ShaderOutputVar -> Bool) -> ShaderOutput a -> [ShaderVarRep]
collectOutput fn = map getOutputVar . filter fn . getOutputVars

collectCustom :: ShaderOutput a -> [ShaderVarRep]
collectCustom = collectOutput isCustom
  where
    isCustom (CustomOutput _ _) = True
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

updateStmt :: ShaderOutputVar -> Statement -> [Statement]
updateStmt (SpecialOutput _ _) s = [s]
updateStmt (CustomOutput _ v1) s@(LocalDecl v2 Nothing)
  | v1 == v2 = []
  | otherwise = [s]
updateStmt (CustomOutput name v1) s@(LocalDecl v2 (Just e))
  | v1 == v2 = [Assignment v e]
  | otherwise = [s]
    where
      v = v1 { shdrVarName = (varyingPrefix ++ name) }
updateStmt _ s@(Assignment _ _) = [s]
updateStmt _ s@(SpecialAssignment _ _) = [s]
updateStmt v (IfThenElse e s1 s2) =
  let output = ShaderOutput [v]
  in [IfThenElse e (updateStmts s1 output) (updateStmts s2 output)]

updateStmts :: [Statement] -> ShaderOutput a -> [Statement]
updateStmts stmts vars =
  let updateFor :: ShaderOutputVar -> [Statement] -> [Statement]
      updateFor v = concat . map (updateStmt v)
  in foldl (flip updateFor) stmts (getOutputVars vars) ++ (mkSpecialStmts vars)

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
ifThen (Expr e) (ShdrCtx c1) (ShdrCtx c2) =
  ShdrCtx $ RWST $ \ipt st ->
  case runRWST c1 ipt (st + 1) of
    Nothing ->
      case runRWST c2 ipt (st + 1) of
        Nothing -> Nothing
        Just (_, id2, (decls2, s2)) -> Just ((), id2 + 1, (decls2, [IfThenElse e [] s2]))
    Just (_, id1, (decls1, s1)) ->
      case runRWST c2 ipt (id1 + 1) of
        Nothing -> Just ((), id1 + 1, (decls1, [IfThenElse e s1 []]))
        Just (_, id2, (decls2, s2)) ->
          Just ((), id2 + 1, (decls1 ++ decls2, [IfThenElse e s1 s2]))

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

getInput :: ShaderVarTy a -> String -> ShaderContext i (ShaderVar a)
getInput (ShaderVarTy expected) name = do
  (ShaderInput vars, shaderTy) <- ask
  let varMap = Map.fromList $ map (\v@(ShdrVarRep n _ _) -> (n, v)) vars
      varName =
        case shaderTy of
          VertexShaderTy -> Map.lookup name varMap
          FragmentShaderTy -> Map.lookup (varyingPrefix ++ name) varMap
  case varName
    of Nothing -> error $ "Unknown shader attribute: " ++ name
       (Just v@(ShdrVarRep _ _ ty))
         | ty == expected -> return (ShaderVar v)
         | otherwise -> error $
                        concat ["Type mismatch for attribute ", show name,
                                ": Expected ", show expected, " got ", show ty]

getInputi :: String -> ShaderContext i (ShaderVar Int)
getInputi = getInput (ShaderVarTy IntTy)

getInputf :: String -> ShaderContext i (ShaderVar Float)
getInputf = getInput (ShaderVarTy FloatTy)

getInput2f :: String -> ShaderContext i (ShaderVar (V2 Float))
getInput2f = getInput (ShaderVarTy Vector2Ty)

getInput3f :: String -> ShaderContext i (ShaderVar (V3 Float))
getInput3f = getInput (ShaderVarTy Vector3Ty)

getInput4f :: String -> ShaderContext i (ShaderVar (V4 Float))
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
  let
      vs_input@(ShaderInput vs_input_vars) = mkAttributes iptTy

      Just (vs_output, varID, (vs_decls, vs_stmts)) = runRWST
                                                      (compileShdrCode vertexPrg)
                                                      (vs_input, VertexShaderTy)
                                                      (length vs_input_vars)

      vs_output_vars = collectCustom vs_output

      extra_vs_stmts =
        let (ShaderOutput vs_out) = vs_output
            isVSInput (SpecialOutput _ _) = False
            isVSInput (CustomOutput _ v) = v `elem` vs_input_vars

            toVSOutput var@(CustomOutput _ v) = Assignment (getOutputVar var) (VarExpr v)
            toVSOutput _ = error "Lambency.Shader.Program (compileProgram): Only output that matches input should go here."
        in map toVSOutput $ filter isVSInput vs_out

      fs_input_vars = vs_output_vars

      fs_input = ShaderInput fs_input_vars

      Just (fs_output, _, (fs_decls, fs_stmts)) = runRWST
                                                  (compileShdrCode fragmentPrg)
                                                  (fs_input, FragmentShaderTy)
                                                  (varID + length fs_input_vars)

      final_vs_stmts = extra_vs_stmts ++ updateStmts vs_stmts vs_output
      final_fs_stmts = updateStmts fs_stmts fs_output

      varyingDecls = map Varying $ filter (flip isShaderVarUsed final_fs_stmts) fs_input_vars
      attribDecls = map Attribute $ filter (flip isShaderVarUsed final_vs_stmts) vs_input_vars
  in
   Shader {
     vertexProgram = ShaderProgram (concat [attribDecls, vs_decls, varyingDecls]) final_vs_stmts,
     fragmentProgram = ShaderProgram (fs_decls ++ varyingDecls) final_fs_stmts
     }
    
