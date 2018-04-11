module Lambency.Shader.OpenGL (
  generateOpenGLShader
) where

--------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Shader.Base
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

unaryOpBS :: UnaryInfix -> String
unaryOpBS Negate = "-"

unaryFnBS :: UnaryFun -> String
unaryFnBS Floor = "floor"
unaryFnBS Ceiling = "ceil"
unaryFnBS Sine = "sin"
unaryFnBS Cosine = "cos"
unaryFnBS Normalize = "normalize"
unaryFnBS Length = "length"
unaryFnBS Fract = "fract"
unaryFnBS CastFloat = "float"

buildUnary :: UnaryOp -> ExprRep -> BS.ByteString
buildUnary (UnaryInfixOp op) e = BS.concat [BS.pack $ unaryOpBS op ++ "(", buildExpr e, BS.pack ")"]
buildUnary (UnaryFunOp op) e = BS.concat [BS.pack $ unaryFnBS op ++ "(", buildExpr e, BS.pack ")"]

--------------------------------------------------

binOpBS :: BinaryInfix -> String
binOpBS Add = "+"
binOpBS Sub = "-"
binOpBS Mult = "*"
binOpBS Div = "/"
binOpBS LessThan = "<"
binOpBS GreaterThan = ">"

binFnBS :: BinaryFunction -> String
binFnBS Pow = "pow"
binFnBS Max = "max"
binFnBS Min = "min"
binFnBS Dot = "dot"
binFnBS Sample1D = "texture1D"
binFnBS Sample2D = "texture2D"
binFnBS Sample3D = "texture3D"
binFnBS Shadow2D = "shadow2D"

buildBinary :: BinaryOp -> ExprRep -> ExprRep -> BS.ByteString

buildBinary (BinaryInfixOp op) e1 e2 =
  BS.concat [BS.pack "(", buildExpr e1,
             BS.pack $ concat [") ", binOpBS op, " ("],
             buildExpr e2, BS.pack ")"]

buildBinary (BinaryFunOp fn) e1 e2 =
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

buildVecExpr :: VecExpr -> BS.ByteString
buildVecExpr (Vec2Expr e1 e2) = BS.concat [BS.pack "vec2(", buildExpr e1, BS.pack ", ",
                                           buildExpr e2, BS.pack ")"]

buildVecExpr (Vec3Expr e1 e2 e3) = BS.concat [BS.pack "vec3(",
                                                buildExpr e1, BS.pack ", ",
                                                buildExpr e2, BS.pack ", ",
                                                buildExpr e3, BS.pack ")"]

buildVecExpr (Vec4Expr e1 e2 e3 e4) = BS.concat [BS.pack "vec4(",
                                                   buildExpr e1, BS.pack ", ",
                                                   buildExpr e2, BS.pack ", ",
                                                   buildExpr e3, BS.pack ", ",
                                                   buildExpr e4, BS.pack ")"]

--------------------------------------------------

buildExpr :: ExprRep -> BS.ByteString
buildExpr (VarExpr v) = BS.pack $ varName v
buildExpr (ConstExpr c) = buildConstant c
buildExpr (SwizzleExpr e sw) = BS.concat [BS.pack "(", buildExpr e, BS.pack ").", buildSwizzle sw]
buildExpr (Unary op e) = buildUnary op e
buildExpr (Binary op e1 e2) = buildBinary op e1 e2
buildExpr (Ternary op e1 e2 e3) = buildTernary op e1 e2 e3
buildExpr (NewVec ve) = buildVecExpr ve

buildStatement :: Int -> Statement -> BS.ByteString
buildStatement indent = flip BS.append (BS.pack ";\n") . buildStmt
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
          buildStatements (indent + 2) s1,
          BS.pack $ take indent (repeat ' ') ++ "} else {\n",
          buildStatements (indent + 2) s2,
          BS.pack $ take indent (repeat ' ') ++ "}"]
        buildStmt (SpecialAssignment VertexPosition v) =
          BS.pack $ concat ["gl_Position = ", varName v]
        buildStmt (SpecialAssignment FragmentColor v) =
          BS.pack $ concat ["gl_FragColor = ", varName v]

buildStatements :: Int -> [Statement] -> BS.ByteString
buildStatements indent stmts =
  BS.concat $ map ((BS.append $ BS.pack $ take indent $ repeat ' ') . buildStatement indent) stmts

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
    cvtTyRep Shadow2DTy = "sampler2DShadow"
    cvtTyRep _ = error "Lambency.Shader.OpenGL -- varTy -- cvtTyRep:  Not implemented!"

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
varDeclaration v@(ShdrVarRep _ _ ty) = BS.append (varTy ty) (BS.pack $ ' ' : (varName v))

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
  in BS.concat $ map buildDeclaration decls ++ [BS.singleton '\n']

buildOpenGLSource :: ShaderProgram -> BS.ByteString
buildOpenGLSource (ShaderProgram decls stmts) =
  BS.concat [BS.pack "#version 120\n",
             buildDeclarations decls,
             BS.pack "void main() {\n",
             buildStatements 2 stmts,
             BS.pack "}"]

printShaderSrc :: BS.ByteString -> IO ()
printShaderSrc shdrSrc = putStrLn $ BS.unpack numberedSrc
 where
   numStrs = map (BS.pack . take 5 . flip (++) ":      " . show) [1::Int,2..]
   numberedSrc = BS.intercalate (BS.singleton '\n') $
                 zipWith BS.append numStrs $ BS.lines shdrSrc

generateShader :: ShaderProgram -> GL.ShaderType -> IO (GL.Shader)
generateShader prg ty = do
  shdr <- GL.createShader ty
  let shdrSrc = buildOpenGLSource prg
  GL.shaderSourceBS shdr GL.$= shdrSrc
  GL.compileShader shdr
  success <- GL.get $ GL.compileStatus shdr
  -- printShaderSrc shdrSrc
  case success of
    True -> return ()
    False -> do
      putStrLn (replicate 80 '-')
      shaderLog <- GL.get $ GL.shaderInfoLog shdr
      putStrLn shaderLog
      printShaderSrc shdrSrc
      putStrLn (replicate 80 '-')
      error "Internal Error: OpenGL shader compilation failed!"
  return shdr

toHighLevel :: ShaderVarTyRep -> L.ShaderValue
toHighLevel Matrix2Ty = L.Matrix2Val identity
toHighLevel Matrix3Ty = L.Matrix3Val identity
toHighLevel Matrix4Ty = L.Matrix4Val identity
toHighLevel Matrix3ListTy = L.Matrix3ListVal []
toHighLevel Matrix4ListTy = L.Matrix4ListVal []
toHighLevel Vector2Ty = L.Vector2Val zero
toHighLevel Vector3Ty = L.Vector3Val zero
toHighLevel Vector4Ty = L.Vector4Val zero
toHighLevel Vector2ListTy = L.Vector2ListVal []
toHighLevel Vector3ListTy = L.Vector3ListVal []
toHighLevel Vector4ListTy = L.Vector4ListVal []
toHighLevel IntTy = L.IntVal 0
toHighLevel IntListTy = L.IntListVal []
toHighLevel FloatTy = L.FloatVal 0
toHighLevel FloatListTy = L.FloatListVal []
toHighLevel Sampler1DTy = L.TextureVal undefined undefined
toHighLevel Sampler2DTy = L.TextureVal undefined undefined
toHighLevel Sampler3DTy = L.TextureVal undefined undefined
toHighLevel Shadow2DTy = L.ShadowMapVal undefined undefined

lookupUniform :: GL.Program -> Declaration -> IO (String, L.ShaderVar)
lookupUniform prg (Uniform v@(ShdrVarRep n _ ty)) = do
  uloc <- GL.get $ GL.uniformLocation prg (varName v)
  if uloc == (GL.UniformLocation (-1))
    then error $ concat ["Internal Error: Did not find uniform "
                        , n, " of type ", show ty
                        ]
    else return (n, L.Uniform (toHighLevel ty) uloc)
lookupUniform _ _ = error "Internal error: Is not a uniform!"

lookupAttrib :: GL.Program -> Declaration -> IO (String, L.ShaderVar)
lookupAttrib prg (Attribute v@(ShdrVarRep n _ ty)) = do
  aloc <- GL.get $ GL.attribLocation prg (varName v)
  if aloc == (GL.AttribLocation (-1))
    then error $ concat ["Internal Error: Did not find attribute "
                        , n, " of type ", show ty
                        ]
    else return (n, L.Attribute (toHighLevel ty) aloc)
lookupAttrib _ _ = error "Internal error: Is not an attribute!"

genVariableLocs :: GL.Program -> [Declaration] -> IO (L.ShaderMap)
genVariableLocs prg decls =
  let ufrms = filter ((== UniformDeclTy) . getDeclType) decls
      attribs = filter ((== AttributeDeclTy) . getDeclType) decls
  in do
    attrMap <- mapM (lookupAttrib prg) attribs
    ufrmMap <- mapM (lookupUniform prg) ufrms
    return $ Map.union (Map.fromList attrMap) (Map.fromList ufrmMap)

generateOpenGLShader :: Shader -> IO (L.Shader)
generateOpenGLShader (Shader vs@(ShaderProgram vs_decls _) fs@(ShaderProgram fs_decls _)) = do
  prg <- GL.createProgram
  generateShader vs GL.VertexShader >>= GL.attachShader prg
  generateShader fs GL.FragmentShader >>= GL.attachShader prg
  GL.linkProgram prg

  vars <- genVariableLocs prg (vs_decls ++ fs_decls)
  
  return $ L.Shader prg vars
