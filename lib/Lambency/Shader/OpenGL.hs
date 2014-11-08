module Lambency.Shader.OpenGL (
  generateOpenGLShader
) where

--------------------------------------------------------------------------------
import Control.Monad (zipWithM)

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Shader.Var
import Lambency.Shader.Expr
import Lambency.Shader.Program

import qualified Lambency.Types as L

import Linear
--------------------------------------------------------------------------------

buildConstant :: Constant -> BS.ByteString
buildConstant (ConstMat2 _) = error "Unable to specify const mat2 variable"
buildConstant (ConstMat3 _) = error "Unable to specify const mat3 variable"
buildConstant (ConstMat4 _) = error "Unable to specify const mat4 variable"

buildConstant (ConstVec2f (V2 x y)) =
  BS.pack $ concat ["vec2(", show x, ", ", show y, ")"]

buildConstant (ConstVec3f (V3 x y z)) =
  BS.pack $ concat ["vec3(", show x, ", ", show y, ", ", show z, ")"]

buildConstant (ConstVec4f (V4 x y z w)) =
  BS.pack $ concat ["vec4(", show x, ", ", show y, ", ", show z, ", ", show w, ")"]

buildConstant (ConstVec2i (V2 x y)) =
  BS.pack $ concat ["ivec2(", show x, ", ", show y, ")"]

buildConstant (ConstVec3i (V3 x y z)) =
  BS.pack $ concat ["ivec3(", show x, ", ", show y, ", ", show z, ")"]

buildConstant (ConstVec4i (V4 x y z w)) =
  BS.pack $ concat ["ivec4(", show x, ", ", show y, ", ", show z, ", ", show w, ")"]

buildConstant (ConstFloat x) = BS.pack $ show x
buildConstant (ConstInt x) = BS.pack $ show x

--------------------------------------------------

getSwizzleBS :: SwizzleVar -> BS.ByteString
getSwizzleBS SwizzleX = BS.pack "x"
getSwizzleBS SwizzleY = BS.pack "y"
getSwizzleBS SwizzleZ = BS.pack "z"
getSwizzleBS SwizzleW = BS.pack "w"

buildSwizzle :: (SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar, Maybe SwizzleVar) -> BS.ByteString
buildSwizzle (v1, Nothing, Nothing, Nothing) = getSwizzleBS v1
buildSwizzle (v1, Just v2, Nothing, Nothing) = BS.concat $ map getSwizzleBS [v1, v2]
buildSwizzle (v1, Just v2, Just v3, Nothing) = BS.concat $ map getSwizzleBS [v1, v2, v3]
buildSwizzle (v1, Just v2, Just v3, Just v4) = BS.concat $ map getSwizzleBS [v1, v2, v3, v4]
buildSwizzle _ = error "Internal error: Swizzle function built incorrectly!"

--------------------------------------------------

buildUnary :: UnaryOp -> ExprRep -> BS.ByteString

buildUnary (UnaryInfixOp Negate) e = BS.concat [BS.pack "-(", buildExpr e, BS.pack ")"]

buildUnary (UnaryFunOp Floor) e = BS.concat [BS.pack "floor(", buildExpr e, BS.pack ")"]
buildUnary (UnaryFunOp Ceiling) e = BS.concat [BS.pack "ceil(", buildExpr e, BS.pack ")"]
buildUnary (UnaryFunOp Sine) e = BS.concat [BS.pack "sin(", buildExpr e, BS.pack ")"]
buildUnary (UnaryFunOp Cosine) e = BS.concat [BS.pack "cos(", buildExpr e, BS.pack ")"]

--------------------------------------------------

binOpBS :: BinaryInfix -> String
binOpBS Add = "+"
binOpBS Sub = "-"
binOpBS Mult = "*"
binOpBS Div = "/"

binFnBS :: BinaryFunction -> String
binFnBS Max = "max"
binFnBS Min = "min"
binFnBS Dot = "dot"

buildBinary :: BinaryOp -> ExprRep -> ExprRep -> BS.ByteString

buildBinary (BinaryInfixOp op) e1 e2 =
  BS.concat [BS.pack "(", buildExpr e1,
             BS.pack $ concat [") ", binOpBS op, " ("],
             buildExpr e2, BS.pack ")"]

buildBinary (BinaryFunctionOp fn) e1 e2 =
  BS.concat [BS.pack (binFnBS fn ++ "("), buildExpr e1, BS.pack ", ", buildExpr e2, BS.pack ")"]

--------------------------------------------------

ternFnBS :: TernaryOp -> String
ternFnBS Clamp = "clamp"
ternFnBS Mix = "mix"

buildTernary :: TernaryOp -> ExprRep -> ExprRep -> ExprRep -> BS.ByteString
buildTernary fn e1 e2 e3 = BS.concat [
  BS.pack (ternFnBS fn ++ "("),
  buildExpr e1,
  BS.pack ", ",
  buildExpr e2,
  BS.pack ", ",
  buildExpr e3,
  BS.pack ")"]

--------------------------------------------------

buildExpr :: ExprRep -> BS.ByteString
buildExpr (VarExpr v) = BS.pack $ varName v
buildExpr (ConstExpr c) = buildConstant c
buildExpr (SwizzleExpr e sw) = BS.concat [BS.pack "(", buildExpr e, BS.pack ").", buildSwizzle sw]
buildExpr (Unary op e) = buildUnary op e
buildExpr (Binary op e1 e2) = buildBinary op e1 e2
buildExpr (Ternary op e1 e2 e3) = buildTernary op e1 e2 e3

buildStatement :: Statement -> BS.ByteString
buildStatement = flip BS.append (BS.pack ";\n") . buildStmt
  where buildStmt (LocalDecl v (Just e)) = BS.concat [
          varDeclaration v,
          BS.pack " = ",
          buildExpr e]
        buildStmt (LocalDecl v Nothing) = varDeclaration v
        buildStmt (Assignment v e) = BS.concat [BS.pack $ varName v ++ " = ", buildExpr e]
        buildStmt (IfThenElse e s1 s2) = BS.concat [
          BS.pack "if (",
          buildExpr e,
          BS.pack ") {\n",
          buildStatements s1,
          BS.pack "} else {\n",
          buildStatements s2,
          BS.pack "}\n"]

buildStatements :: [Statement] -> BS.ByteString
buildStatements stmts = BS.concat $
                        map ((BS.append $ BS.pack "  ") . buildStatement) stmts

varTy :: ShaderVarTyRep -> BS.ByteString
varTy = BS.pack . cvtTyRep
  where
    cvtTyRep :: ShaderVarTyRep -> String
    cvtTyRep Matrix2Ty = "mat2"
    cvtTyRep Matrix3Ty = "mat3"
    cvtTyRep Matrix4Ty = "mat4"
    cvtTyRep Vector2Ty = "vec2"
    cvtTyRep Vector3Ty = "vec3"
    cvtTyRep Vector4Ty = "vec4"
    cvtTyRep IntTy = "int"
    cvtTyRep FloatTy = "float"
    cvtTyRep Sampler1DTy = "sampler1D"
    cvtTyRep Sampler2DTy = "sampler2D"
    cvtTyRep Sampler3DTy = "sampler3D"
    cvtTyRep _ = error "Not implemented!"

{-- !FIXME! what was I thinking here?    
    cvtTyRep IntListTy = "IntListTy"
    cvtTyRep FloatListTy = "FloatListTy"
    cvtTyRep Matrix3ListTy = "Matrix3ListTy"
    cvtTyRep Matrix4ListTy = "Matrix4ListTy"
    cvtTyRep Vector2ListTy = "Vector2ListTy"
    cvtTyRep Vector3ListTy = "Vector3ListTy"
    cvtTyRep Vector4ListTy = "Vector4ListTy"
--}

varName :: ShaderVarRep -> String
varName (ShdrVarRep n i _) = concat [n, "_", show i]

varDeclaration :: ShaderVarRep -> BS.ByteString
varDeclaration v@(ShdrVarRep _ _ ty) = BS.append (varTy ty) (BS.pack $ varName v)

buildDeclaration :: Declaration -> BS.ByteString
buildDeclaration = flip BS.append (BS.pack ";\n") . declString
  where
    declString :: Declaration -> BS.ByteString
    declString (Attribute v) = BS.append (BS.pack "attribute ") (varDeclaration v)
    declString (Uniform v) = BS.append (BS.pack "uniform ") (varDeclaration v)
    declString (Varying v) = BS.append (BS.pack "varying ") (varDeclaration v)
    declString (ConstDecl v e) = BS.concat [
      BS.pack "const ",
      varDeclaration v,
      BS.pack " = ",
      buildExpr e]

buildDeclarations :: [Declaration] -> BS.ByteString
buildDeclarations decls' =
  let groupDecls x y = getDeclType x == getDeclType y
      decls = concat $ List.groupBy groupDecls decls'
  in BS.concat $ map (BS.cons '\n' . buildDeclaration) decls

buildOpenGLSource :: ShaderProgram -> BS.ByteString
buildOpenGLSource (ShaderProgram decls stmts) =
  BS.append (buildDeclarations decls) $ BS.concat [
    BS.pack "void main() {\n",
    buildStatements stmts,
    BS.pack "}"]

generateShader :: ShaderProgram -> GL.ShaderType -> IO (GL.Shader)
generateShader prg ty = do
  shdr <- GL.createShader ty
  GL.shaderSourceBS shdr GL.$= (buildOpenGLSource prg)
  GL.compileShader shdr
  shaderLog <- GL.get $ GL.shaderInfoLog shdr
  if null shaderLog
    then return ()
    else putStrLn shaderLog
  return shdr

toHighLevelTy :: Int -> ShaderVarTyRep -> L.ShaderVarTy
toHighLevelTy _ Matrix2Ty = L.Matrix2Ty
toHighLevelTy _ Matrix3Ty = L.Matrix3Ty
toHighLevelTy _ Matrix4Ty = L.Matrix4Ty
toHighLevelTy _ Matrix3ListTy = L.Matrix3ListTy
toHighLevelTy _ Matrix4ListTy = L.Matrix4ListTy
toHighLevelTy _ Vector2Ty = error "Not implemented"
toHighLevelTy _ Vector3Ty = L.Vector3Ty
toHighLevelTy _ Vector4Ty = L.Vector4Ty
toHighLevelTy _ Vector2ListTy = error "Not implemented"
toHighLevelTy _ Vector3ListTy = L.Vector3ListTy
toHighLevelTy _ Vector4ListTy = L.Vector4ListTy
toHighLevelTy _ IntTy = L.IntTy
toHighLevelTy _ IntListTy = L.IntListTy
toHighLevelTy _ FloatTy = L.FloatTy
toHighLevelTy _ FloatListTy = L.FloatListTy
toHighLevelTy n Sampler1DTy = L.TextureTy (toEnum n)
toHighLevelTy n Sampler2DTy = L.TextureTy (toEnum n)
toHighLevelTy n Sampler3DTy = L.TextureTy (toEnum n)

isSampler :: ShaderVarTyRep -> Bool
isSampler Sampler1DTy = True
isSampler Sampler2DTy = True
isSampler Sampler3DTy = True
isSampler _ = False

toSamplerIdx :: Int -> Declaration -> Int
toSamplerIdx idx (Uniform (ShdrVarRep _ _ ty)) = if isSampler ty then idx + 1 else idx
toSamplerIdx _ _ = error "Only uniforms have sampler types!"

lookupUniform :: GL.Program -> Int -> Declaration -> IO (String, L.ShaderVar)
lookupUniform prg idx (Uniform v@(ShdrVarRep n _ ty)) = do
  uloc <- GL.get $ GL.uniformLocation prg (varName v)
  if uloc == (GL.UniformLocation (-1))
    then error $ concat ["Internal Error: Did not find uniform ", n, " of type ", show ty]
    else return (n, L.Uniform (toHighLevelTy idx ty) uloc)
lookupUniform _ _ _ = error "Internal error: Is not a uniform!"

lookupAttrib :: GL.Program -> Declaration -> IO (String, L.ShaderVar)
lookupAttrib prg (Attribute v@(ShdrVarRep n _ ty)) = do
  aloc <- GL.get $ GL.attribLocation prg (varName v)
  if aloc == (GL.AttribLocation (-1))
    then error $ concat ["Internal Error: Did not find attribute ", n, " of type ", show ty]
    else return (n, L.Attribute (toHighLevelTy undefined ty) aloc)
lookupAttrib _ _ = error "Internal error: Is not an attribute!"

genVariableLocs :: GL.Program -> [Declaration] -> IO (Map.Map String L.ShaderVar)
genVariableLocs prg decls =
  let ufrms = filter ((== UniformDeclTy) . getDeclType) decls
      attribs = filter ((== AttributeDeclTy) . getDeclType) decls
  in do
    attrMap <- mapM (lookupAttrib prg) attribs
    ufrmMap <- zipWithM (lookupUniform prg) (scanl toSamplerIdx 0 ufrms) ufrms
    return $ Map.union (Map.fromList attrMap) (Map.fromList ufrmMap)

generateOpenGLShader :: Shader v -> IO (L.Shader)
generateOpenGLShader (Shader vs@(ShaderProgram vs_decls _) fs@(ShaderProgram fs_decls _)) = do
  prg <- GL.createProgram
  generateShader vs GL.VertexShader >>= GL.attachShader prg
  generateShader fs GL.FragmentShader >>= GL.attachShader prg
  GL.linkProgram prg

  vars <- genVariableLocs prg (vs_decls ++ fs_decls)
  
  return $ L.Shader prg vars
