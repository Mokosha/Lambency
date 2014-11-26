module Lambency.Loaders.MTLLoader (
  MTL(..),
  mkMaterial,
  loadMTL,
) where

--------------------------------------------------------------------------------
import Control.Applicative hiding ((<|>), many)

import Data.Char (toLower)
import Data.List (find)
import Data.Text (pack)

import qualified Lambency.Texture as L
import qualified Lambency.Material as L
import qualified Lambency.Types as L

import Linear hiding (trace)

import System.FilePath

import Text.Parsec
import Text.Parsec.Text (Parser)
--------------------------------------------------------------------------------

type Vec3f = V3 Float

{--
  Ft Fresnel reflectance
  Ft Fresnel transmittance
  Ia ambient light
  I  light intensity
  Ir intensity from reflected direction (reflection map and/or ray tracing)
  It intensity from transmitted direction
  Ka ambient reflectance
  Kd diffuse reflectance
  Ks specular reflectance
  Tf transmission filter 

  H unit vector bisector between L and V
  L unit light vector
  N unit surface normal
  V unit view vector 
--}

data IlluminationMode
  -- This is a constant color illumination model. The color is the specified Kd
  -- for the material.
  = IlluminationMode'ColorOnAmbientOff

  -- This is a diffuse illumination model using Lambertian shading. The color
  -- includes an ambient constant term and a diffuse shading term for each light
  -- source:
  -- color = Ka Ia + Kd { SUM j=1..ls, (N * Lj)Ij } 
  | IlluminationMode'ColorOnAmbientOn

  -- This is a diffuse and specular illumination model using Lambertian shading
  -- and Blinn's interpretation of Phong's specular illumination model (BLIN77).
  -- The color includes an ambient constant term, and a diffuse and specular
  -- shading term for each light source:
  -- color = KaIa + Kd { SUM j=1..ls, (N*Lj)Ij } + Ks { SUM j=1..ls, ((N*Hj)^Ns)Ij } 
  | IlluminationMode'HighlightOn

  -- This is a diffuse and specular illumination model using Lambertian shading
  -- and Blinn's interpretation of Phong's specular illumination model (BLIN77).
  -- The color includes an ambient constant term, and a diffuse and specular shading
  -- term for each light source:
  -- color = Ka Ia + Kd { SUM j=1..ls, (N*Lj)Ij } + Ks ({ SUM j=1..ls, ((N*Hj)^Ns)Ij } + Ir) 
  | IlluminationMode'ReflectionRayTrace

  -- According to the MTL spec, this is identical to IlluminationMode'ReflectionRayTrace
  | IlluminationMode'GlassRayTrace

  -- The rest aren't supported yet...
  | IlluminationMode'FresnelRayTrace
  | IlluminationMode'RefractionRayTrace
  | IlluminationMode'RefractionFresnelRayTrace
  | IlluminationMode'Reflection
  | IlluminationMode'Glass
  | IlluminationMode'ShadowCaster
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data ColorChannel
  = ColorChannel'Red
  | ColorChannel'Green
  | ColorChannel'Blue
  | ColorChannel'Matte
  | ColorChannel'Luma
  | ColorChannel'Depth
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data TextureInfo = TextureInfo {
  horizBlend :: Bool,     -- Default: True
  vertBlend :: Bool,      -- Default: True
  clampUV :: Bool,        -- Default: False
  channelRestriction :: Maybe ColorChannel,
  valueRange :: (Float, Float),  -- Default: (0, 1)
  texTranslate :: V3 Float, -- Default: (0, 0, 0)
  texScale :: V3 Float,     -- Default: (1, 1, 1)
  texTurbulence :: V3 Float -- Default: (0, 0, 0)
}
 deriving (Show, Eq, Ord)

data ReflectionType
  = ReflectionType'Sphere
  | ReflectionType'CubeTop
  | ReflectionType'CubeBottom
  | ReflectionType'CubeFront
  | ReflectionType'CubeBack
  | ReflectionType'CubeLeft
  | ReflectionType'CubeRight
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data TextureMap
  = ColorMap {
    _colorTexInfo :: TextureInfo,
    _colorCorrection :: Bool
  }
  | BumpMap {
    _bumpTexInfo :: TextureInfo,
    _multiplier :: Float
  }
  | DecalMap {
    _decalMapInfo :: TextureInfo
  }
  | DisplacementMap {
    _dispMapInfo :: TextureInfo
  }
  | ReflectionMap {
    _reflMapInfo :: TextureInfo,
    _colorCorrection :: Bool,
    _reflTy :: ReflectionType
  }
 deriving (Show, Eq, Ord)

data ReflectivityInfo = ReflectivityInfo {
  reflColor :: Maybe (V3 Float),
  reflMap :: Maybe (FilePath, TextureMap)
}
 deriving (Show, Eq, Ord)

data DissolveInfo = DissolveInfo {
  _dissolveHalo :: Bool,
  _dissolveFactor :: Float
}
 deriving (Show, Eq, Ord)

data MTL = MTL {
  mtlName :: String,
  ambientInfo :: ReflectivityInfo,
  diffuseInfo :: ReflectivityInfo,
  specularInfo :: ReflectivityInfo,

  emissiveColor :: Maybe (V3 Float),

  -- The Tf statement specifies the transmission filter using RGB values. 

  -- "r g b" are the values for the red, green, and blue components of the atmosphere.
  -- The g and b arguments are optional. If only r is specified, then g, and b are
  -- assumed to be equal to r. The r g b values are normally in the range of 0.0 to 1.0.
  -- Values outside this range increase or decrease the relectivity accordingly. 
  transferrence :: V3 Float,

  illuminationMode :: IlluminationMode,

  dissolve :: DissolveInfo,

  specularExponent :: Float,
  sharpness :: Float,
  indexOfRefraction :: Float,

  textureMaps :: [(FilePath, TextureMap)]
}
 deriving (Show, Eq, Ord)

----------------------------------------------------------------------
-- Interface with Lambency

type LReflectivity = (L.MaterialVar (V3 Float), L.MaterialVar L.Texture)

updateReflectivity :: FilePath -> ReflectivityInfo -> LReflectivity -> IO (LReflectivity)
updateReflectivity baseDir reflInfo (v1, v2) = do
  let v1n = L.getMatVarName v1
      v2n = L.getMatVarName v2

      v1' = case reflColor reflInfo of
        Nothing -> Nothing
        Just v -> Just $ L.Vector3Val v

  v2' <- case reflMap reflInfo of
    Nothing -> return Nothing
    Just (fp, _) -> do
      tex <- L.loadTexture $ baseDir </> fp
      case tex of
        Nothing -> return Nothing
        Just t -> return . Just $ L.TextureVal t

  return (L.MaterialVar (v1n, v1'), L.MaterialVar (v2n, v2'))

mkMaterial :: FilePath -> MTL -> IO (L.Material)
mkMaterial baseDir mtl = do
  let initialMat = L.defaultBlinnPhong

  (dRefl, dMap) <- updateReflectivity baseDir (diffuseInfo mtl)
                   (L.diffuseReflectivity initialMat, L.diffuseMap initialMat)

  (sRefl, sMap) <- updateReflectivity baseDir (specularInfo mtl)
                   (L.specularReflectivity initialMat, L.specularMap initialMat)

  let aRefl = case ambientInfo mtl of
        ReflectivityInfo Nothing _ -> L.ambientReflectivity initialMat
        ReflectivityInfo (Just c) _ ->
          let (L.MaterialVar (n, _)) = L.ambientReflectivity initialMat
          in L.MaterialVar (n, Just $ L.Vector3Val c)

      sExp = let (L.MaterialVar (n, _)) = L.specularExponent initialMat
              in (L.MaterialVar (n, Just $ L.FloatVal $ specularExponent mtl))

  return $
    initialMat {
      L.diffuseReflectivity = dRefl,
      L.diffuseMap = dMap,

      L.specularExponent = sExp,
      L.specularReflectivity = sRefl,
      L.specularMap = sMap,

      L.ambientReflectivity = aRefl
      }

----------------------------------------------------------------------
-- Parser

float :: Parser Float
float = do
  spaces
  sign <- option 1 $ (\s -> if s == '-' then (-1.0) else 1.0) <$> oneOf "+-"
  t <- option "0" $ many digit
  _ <- if t == [] then (char '.') else ((try $ char '.') <|> (return ' '))
  d <- option "0" $ many1 digit
  let
    denom :: Float
    denom = if d == "0" then 1.0 else (fromIntegral $ length d)
  e <- option "0" $ char 'e' >> (many1 digit)

  return $ ((read t) + ((read d) / (10 ** denom))) * (10 ** (read e)) * sign

vector3 :: Parser Vec3f
vector3 = V3 <$> float <*> float <*> float

data IllumCommand
  = IllumCommand'AmbientReflectivity (V3 Float)
  | IllumCommand'DiffuseReflectivity (V3 Float)
  | IllumCommand'SpecularReflectivity (V3 Float)
  | IllumCommand'Emissive (V3 Float)
  | IllumCommand'Transferrence (V3 Float)
  | IllumCommand'Mode IlluminationMode
  | IllumCommand'Dissolve DissolveInfo
  | IllumCommand'SpecularExponent Float
  | IllumCommand'Sharpness Float
  | IllumCommand'IndexOfRefraction Float
    deriving (Show, Eq, Ord)

data TextureMapCommand
  = TextureMapCommand'Ambient (FilePath, TextureMap)
  | TextureMapCommand'Diffuse (FilePath, TextureMap)
  | TextureMapCommand'Specular (FilePath, TextureMap)
  | TextureMapCommand'SpecularExponent (FilePath, TextureMap)
  | TextureMapCommand'Dissolve (FilePath, TextureMap)
  | TextureMapCommand'Reflection (FilePath, TextureMap)
  | TextureMapCommand'Decal (FilePath, TextureMap)
  | TextureMapCommand'Disp (FilePath, TextureMap)
  | TextureMapCommand'Bump (FilePath, TextureMap)
    deriving (Show, Eq, Ord)

data TextureInfoCommand
  = TextureInfoCommand'HorizBlend Bool
  | TextureInfoCommand'VertBlend Bool
  | TextureInfoCommand'ClampUV Bool
  | TextureInfoCommand'ChannelRestriction ColorChannel
  | TextureInfoCommand'ValueRange (Float, Float)
  | TextureInfoCommand'Translate (V3 Float)
  | TextureInfoCommand'Scale (V3 Float)
  | TextureInfoCommand'Turbulence (V3 Float)

  | TextureInfoCommand'ColorCorrection Bool
  | TextureInfoCommand'BumpMultiplier Float

  | TextureInfoCommand'ReflectionType ReflectionType
    deriving (Show, Eq, Ord)

whitespace :: Parser Char
whitespace = tab <|> char ' '

whitespaces :: Parser [Char]
whitespaces = many whitespace

finishLine :: Parser ()
finishLine = many (whitespace <|> comment) >> endMarker >> return ()
  where
    endMarker = endOfLine
    
    comment :: Parser Char
    comment = char '#' >> manyTill anyChar (try $ lookAhead endMarker) >> return '_'

parseTexOpts :: Parser [TextureInfoCommand]
parseTexOpts = many texOpt
  where

    parseBoolOpt :: String -> Parser Bool
    parseBoolOpt s = try $ whitespaces >> char '-' >> string s >> whitespaces >>
                     ((string "on" >> return True) <|> (string "off" >> return False))

    parsePartialV3Opt :: Char -> Parser Vec3f
    parsePartialV3Opt c = try $ whitespaces >> char '-' >> char c >> (do
      x <- float
      y <- option x float
      z <- option x float
      return $ V3 x y z)

    parseFloatOpt :: String -> Parser Float
    parseFloatOpt s = try $ whitespaces >> char '-' >> string s >> float

    parseValueRange :: Parser (Float, Float)
    parseValueRange = try $ whitespaces >> string "-mm" >> (do
      base <- float
      gain <- float
      return (base, gain))

    parseBumpChannel :: Parser ColorChannel
    parseBumpChannel = try $ whitespaces >> string "-imfchan" >>
      ((char 'r' >> return ColorChannel'Red) <|>
       (char 'g' >> return ColorChannel'Green) <|>
       (char 'b' >> return ColorChannel'Blue) <|>
       (char 'm' >> return ColorChannel'Matte) <|>
       (char 'l' >> return ColorChannel'Luma) <|>
       (char 'z' >> return ColorChannel'Depth))

    parseReflectionType :: Parser ReflectionType
    parseReflectionType = try $ whitespaces >> string "-type" >> whitespaces >>
      ((string "sphere" >> return ReflectionType'Sphere) <|>
       (string "cube_top" >> return ReflectionType'CubeTop) <|>
       (string "cube_bottom" >> return ReflectionType'CubeBottom) <|>
       (string "cube_front" >> return ReflectionType'CubeFront) <|>
       (string "cube_back" >> return ReflectionType'CubeBack) <|>
       (string "cube_left" >> return ReflectionType'CubeLeft) <|>
       (string "cube_right" >> return ReflectionType'CubeRight))

    texOpt :: Parser TextureInfoCommand
    texOpt =
      (TextureInfoCommand'HorizBlend <$> parseBoolOpt "blendu")
      <|> (TextureInfoCommand'VertBlend <$> parseBoolOpt "blendv")
      <|> (TextureInfoCommand'ClampUV <$> parseBoolOpt "clamp")
      <|> (TextureInfoCommand'ColorCorrection <$> parseBoolOpt "cc")

      <|> (TextureInfoCommand'Translate <$> parsePartialV3Opt 'o')
      <|> (TextureInfoCommand'Scale <$> parsePartialV3Opt 's')
      <|> (TextureInfoCommand'Turbulence <$> parsePartialV3Opt 't')

      <|> (TextureInfoCommand'BumpMultiplier <$> parseFloatOpt "bm")

      <|> (TextureInfoCommand'ValueRange <$> parseValueRange)

      <|> (TextureInfoCommand'ReflectionType <$> parseReflectionType)

      <|> (TextureInfoCommand'ChannelRestriction <$> parseBumpChannel)

      <|> (try $ string "-texres" >> float >>=
           (\x -> error $ "Lambency.Loaders.MTLLoader (parseTexOpts): Unsupported token (texres)" ++ show x))

handleTexInfo :: [TextureInfoCommand] -> TextureInfo
handleTexInfo =
  let defaultInfo = TextureInfo {
        horizBlend = True,
        vertBlend = True,
        clampUV = False,
        channelRestriction = Nothing,
        valueRange = (0, 1),
        texTranslate = V3 0 0 0,
        texScale = V3 1 1 1,
        texTurbulence = V3 0 0 0
        }

      handleCommand :: TextureInfoCommand -> TextureInfo -> TextureInfo
      handleCommand (TextureInfoCommand'HorizBlend b) i = i { horizBlend = b }
      handleCommand (TextureInfoCommand'VertBlend b) i = i { vertBlend = b }
      handleCommand (TextureInfoCommand'ClampUV b) i = i { clampUV = b }
      handleCommand (TextureInfoCommand'ChannelRestriction x) i = i { channelRestriction = Just x }
      handleCommand (TextureInfoCommand'ValueRange x) i = i { valueRange = x }
      handleCommand (TextureInfoCommand'Translate x) i = i { texTranslate = x}
      handleCommand (TextureInfoCommand'Scale x) i = i { texScale = x }
      handleCommand (TextureInfoCommand'Turbulence x) i = i { texTurbulence = x }

      handleCommand x _ = error $ "Lambency.Loaders.MTLLoader (handleTexInfo): Unexpected info command: " ++ show x

  in foldr handleCommand defaultInfo 

constructMaterial :: String -> [IllumCommand] -> [TextureMapCommand] -> MTL
constructMaterial name illumCmds = foldr handleTexMapCmd illumMtl
  where
    illumMtl = foldr handleIllumCmd defaultMTL illumCmds

    defaultMTL = MTL {
      mtlName = map toLower name,

      ambientInfo = ReflectivityInfo Nothing Nothing,
      diffuseInfo = ReflectivityInfo Nothing Nothing,
      specularInfo = ReflectivityInfo Nothing Nothing,

      emissiveColor = Nothing,

      transferrence = V3 1 1 1,

      illuminationMode = IlluminationMode'HighlightOn,

      dissolve = DissolveInfo False 1.0,

      specularExponent = 10.0,
      sharpness = 1.0,
    
      indexOfRefraction = 1.5,

      textureMaps = []
      }

    handleIllumCmd :: IllumCommand -> MTL -> MTL
    handleIllumCmd (IllumCommand'AmbientReflectivity x) mtl =
      mtl { ambientInfo = (ambientInfo mtl) { reflColor = Just x }}
    handleIllumCmd (IllumCommand'DiffuseReflectivity x) mtl =
      mtl { diffuseInfo = (diffuseInfo mtl) { reflColor = Just x }}
    handleIllumCmd (IllumCommand'SpecularReflectivity x) mtl =
      mtl { specularInfo = (specularInfo mtl) { reflColor = Just x }}
    handleIllumCmd (IllumCommand'Emissive x) mtl = mtl { emissiveColor = Just x }
    handleIllumCmd (IllumCommand'Transferrence x) mtl = mtl { transferrence = x }
    handleIllumCmd (IllumCommand'Mode mode) mtl = mtl { illuminationMode = mode }
    handleIllumCmd (IllumCommand'Dissolve info) mtl = mtl { dissolve = info }
    handleIllumCmd (IllumCommand'SpecularExponent x) mtl = mtl { specularExponent = x }
    handleIllumCmd (IllumCommand'Sharpness x) mtl = mtl { sharpness = x }
    handleIllumCmd (IllumCommand'IndexOfRefraction x) mtl = mtl { indexOfRefraction = x }

    handleTexMapCmd :: TextureMapCommand -> MTL -> MTL
    handleTexMapCmd (TextureMapCommand'Ambient x) mtl =
      mtl { ambientInfo = (ambientInfo mtl) { reflMap = Just x }}
    handleTexMapCmd (TextureMapCommand'Diffuse x) mtl =
      mtl { diffuseInfo = (diffuseInfo mtl) { reflMap = Just x }}
    handleTexMapCmd (TextureMapCommand'Specular x) mtl =
      mtl { specularInfo = (specularInfo mtl) { reflMap = Just x }}
    handleTexMapCmd (TextureMapCommand'SpecularExponent x) mtl = mtl { textureMaps = x : (textureMaps mtl) }
    handleTexMapCmd (TextureMapCommand'Dissolve x) mtl = mtl { textureMaps = x : (textureMaps mtl) }
    handleTexMapCmd (TextureMapCommand'Reflection x) mtl = mtl { textureMaps = x : (textureMaps mtl) }
    handleTexMapCmd (TextureMapCommand'Decal x) mtl = mtl { textureMaps = x : (textureMaps mtl) }
    handleTexMapCmd (TextureMapCommand'Disp x) mtl = mtl { textureMaps = x : (textureMaps mtl) }
    handleTexMapCmd (TextureMapCommand'Bump x) mtl = mtl { textureMaps = x : (textureMaps mtl) }

parseFile :: Parser [MTL]
parseFile = many material >>= (\x -> many finishLine >> eof >> return x)
  where
    readReflectance :: String -> Parser (V3 Float)
    readReflectance s = try $ whitespaces >> string s >> vector3

    parseFloatCmd :: String -> Parser Float
    parseFloatCmd s = try $ whitespaces >> string s >> float

    parseDissolve :: Parser DissolveInfo
    parseDissolve = try $ do
      _ <- whitespaces >> char 'd'
      isHalo <- whitespaces >> ((string "-halo" >> return True) <|> return False)
      val <- float
      return $ DissolveInfo isHalo val

    illum :: Parser IllumCommand
    illum =
      (IllumCommand'AmbientReflectivity <$> (readReflectance "Ka")) <|>
      (IllumCommand'DiffuseReflectivity <$> (readReflectance "Kd")) <|>
      (IllumCommand'SpecularReflectivity <$> (readReflectance "Ks")) <|>
      (IllumCommand'Emissive <$> (readReflectance "Ke")) <|>
      (IllumCommand'Transferrence <$> (readReflectance "Tf")) <|>
      (IllumCommand'Dissolve <$> parseDissolve) <|>
      (IllumCommand'Dissolve . DissolveInfo False . (1.0 -) <$> parseFloatCmd "Tr") <|>
      (IllumCommand'Mode . toEnum . round <$> parseFloatCmd "illum") <|>
      (IllumCommand'SpecularExponent <$> parseFloatCmd "Ns") <|>
      (IllumCommand'Sharpness <$> parseFloatCmd "sharpness") <|>
      (IllumCommand'IndexOfRefraction <$> parseFloatCmd "Ni")

    parseIllum :: Parser [IllumCommand]
    parseIllum = many $ illum >>= (\x -> finishLine >> return x)

    colorMap :: String -> Parser (FilePath, TextureMap)
    colorMap n = try $ do
      infoCmds <- whitespaces >> string n >> parseTexOpts
      let isColorCorrection (TextureInfoCommand'ColorCorrection _) = True
          isColorCorrection _ = False

          correction =
            case find isColorCorrection infoCmds of
              Just (TextureInfoCommand'ColorCorrection b) -> b
              _ -> False

          revisedInfoCmds = filter (not . isColorCorrection) infoCmds

      filename <- whitespaces >> manyTill anyChar (try $ lookAhead space)
      return $ (filename, ColorMap (handleTexInfo revisedInfoCmds) correction)

    reflectionMap :: Parser (FilePath, TextureMap)
    reflectionMap = try $ do
      infoCmds <- whitespaces >> string "refl" >> parseTexOpts
      let isReflectionType (TextureInfoCommand'ReflectionType _) = True
          isReflectionType _ = False

          refl =
            case find isReflectionType infoCmds of
              Just (TextureInfoCommand'ReflectionType rty) -> rty
              _ -> error "Lambency.Loaders.MTLLoader (parseFile): Reflection map must contain reflection type"

          isColorCorrection (TextureInfoCommand'ColorCorrection _) = True
          isColorCorrection _ = False

          correction =
            case find isColorCorrection infoCmds of
              Just (TextureInfoCommand'ColorCorrection b) -> b
              _ -> False

          revisedInfoCmds = filter (not . isReflectionType) $
                            filter (not . isColorCorrection) infoCmds

      filename <- manyTill anyChar (try $ lookAhead space)
      return (filename, ReflectionMap (handleTexInfo revisedInfoCmds) correction refl)

    bumpMap :: Parser (FilePath, TextureMap)
    bumpMap = try $ do
      infoCmds <- whitespaces >> string "bump" >> parseTexOpts
      let isBumpMultiplier (TextureInfoCommand'BumpMultiplier _) = True
          isBumpMultiplier _ = False

          bumpMultiplier =
            case find isBumpMultiplier infoCmds of
              Just (TextureInfoCommand'BumpMultiplier m) -> m
              _ -> 1.0

          revisedInfoCmds = filter (not . isBumpMultiplier) infoCmds
      filename <- manyTill anyChar (try $ lookAhead space)
      return (filename, BumpMap (handleTexInfo revisedInfoCmds) bumpMultiplier)

    decalMap :: Parser (FilePath, TextureMap)
    decalMap = try $ do
      texture <- DecalMap . handleTexInfo <$> (whitespaces >> string "decal" >> parseTexOpts)
      filename <- manyTill anyChar (try $ lookAhead space)
      return (filename, texture)

    dispMap :: Parser (FilePath, TextureMap)
    dispMap = try $ do
      texture <- DisplacementMap . handleTexInfo <$> (whitespaces >> string "disp" >> parseTexOpts)
      filename <- manyTill anyChar (try $ lookAhead space)
      return (filename, texture)

    texMap :: Parser TextureMapCommand
    texMap =
      (TextureMapCommand'Ambient <$> colorMap "map_Ka") <|>
      (TextureMapCommand'Diffuse <$> colorMap "map_Kd") <|>
      (TextureMapCommand'Specular <$> colorMap "map_Ks") <|>
      (TextureMapCommand'SpecularExponent <$> colorMap "map_Ns") <|>
      (TextureMapCommand'Dissolve <$> colorMap "map_d") <|>
      (TextureMapCommand'Reflection  <$> reflectionMap) <|>
      (TextureMapCommand'Decal <$> decalMap) <|>
      (TextureMapCommand'Disp <$> dispMap) <|>
      (TextureMapCommand'Bump <$> bumpMap)

    parseTexMaps :: Parser [TextureMapCommand]
    parseTexMaps = many $ texMap >>= (\x -> finishLine >> return x)

    material :: Parser MTL
    material = try $ do
      name <- many finishLine >> string "newmtl" >> whitespaces >> manyTill anyChar (try finishLine)
      illumCmds <- parseIllum
      texMapCmds <- parseTexMaps
      return $ constructMaterial name illumCmds texMapCmds

parseMTL :: FilePath -> IO [MTL]
parseMTL filepath = do
  s <- readFile filepath
  case parse parseFile filepath (pack s) of
    Left x -> error $ show x
    Right x -> return $ x

loadMTL :: FilePath -> IO [MTL]
loadMTL = parseMTL
