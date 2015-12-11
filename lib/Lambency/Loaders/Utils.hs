module Lambency.Loaders.Utils where

--------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative hiding ((<|>), many)
#endif

import Text.Parsec
import Text.Parsec.Text (Parser)

import Linear.V2
import Linear.V3
--------------------------------------------------------------------------------

type Vec2f = V2 Float
type Vec3f = V3 Float

sign :: Parser Float
sign = option 1 $ do s <- oneOf "+-"
                     return $ if s == '-' then (-1.0) else 1.0

float :: Parser Float
float = do
  spaces
  sgn <- sign
  t <- option "0" $ many digit
  _ <- if t == [] then (char '.') else ((try $ char '.') <|> (return ' '))
  d <- option "0" $ many1 digit
  let
    denom :: Float
    denom = if d == "0" then 1.0 else (fromIntegral $ length d)
  e <- option 0 $ do
    esign <- char 'e' >> sign
    ((*esign) . read) <$> (many1 digit)

  return $ ((read t) + ((read d) / (10 ** denom))) * (10 ** e) * sgn

vector2 :: Parser Vec2f
vector2 = V2 <$> float <*> float

vector3 :: Parser Vec3f
vector3 = V3 <$> float <*> float <*> float

