module Lambency.Font (
  Font,
  loadSystemFont,
  loadFont,
  renderUIString,
) where

--------------------------------------------------------------------------------
import qualified Data.Map as Map

import Lambency.Render
import Lambency.Sprite
import Lambency.Types
import Lambency.Utils

import Linear

import Paths_lambency
import System.FilePath
--------------------------------------------------------------------------------

newtype Font = Font { getGlyph :: Char -> Maybe SpriteFrame }

loadFont :: FilePath -> [(Char, V2 Int, V2 Int)] -> IO (Font)
loadFont filename charInfo = let
  offsets = map (\(_, x, _) -> x) charInfo
  sizes = map (\(_, _, x) -> x) charInfo
  in do
    sprite' <- loadAnimatedSprite filename sizes offsets
    return . Font $ \c -> do
      sprite <- sprite'
      let frames = cyclicToList (getFrames sprite)
          charMap = Map.fromList $ zip (map (\(c', _, _) -> c') charInfo) frames
      c `Map.lookup` charMap

loadSystemFont :: IO (Font)
loadSystemFont = let
  systemOffsets = [V2 x 0 | x <- [0,14..]]
  systemSizes = repeat (V2 13 24)
  systemString = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
  in do
    fontTexFile <- getDataFileName $ "font" <.> "png"
    loadFont fontTexFile (zip3 systemString systemOffsets systemSizes)

renderUIString :: Font -> String -> V2 Float -> GameMonad ()
renderUIString _ "" _ = return ()
renderUIString font str pos = let
  glyphSize :: Char -> V2 Int
  glyphSize c =
    case (getGlyph font c) of
      Nothing -> zero
      Just f -> size f
  
  glyphSizes :: [V2 Int]
  glyphSizes = map glyphSize str
  
  positions :: [V2 Float]
  positions = let
    helper _ _ [] = []
    helper (V2 cx cy) (V2 lx _) ((V2 x y):xs) =
      let nextPos = V2 (cx + (fromIntegral (x + lx) * 0.5)) cy
      in nextPos : (helper nextPos (V2 x y) xs)
   in
    helper pos (head glyphSizes) (tail glyphSizes)
  in do
   mapM_ (\(c, p) ->
           case (getGlyph font c) of
                 Nothing -> return ()
                 Just f -> addRenderUIAction p (frameRO f)) (zip str positions)
