module Lambency.Renderer
  ( nullRenderer
  , openGLRenderer
  , addClippedRenderAction
  , addTransformedRenderAction
  , addRenderAction
  , addRenderUIAction
  ) where

--------------------------------------------------------------------------------
import Control.Arrow (second)
import Control.Monad.RWS.Strict

import qualified Graphics.UI.GLFW as GLFW

import Lambency.Types
import Lambency.Transform

import Linear hiding (identity)

import qualified Lambency.Renderer.OpenGL.Texture as OpenGL
import qualified Lambency.Renderer.OpenGL.Render as OpenGL
--------------------------------------------------------------------------------

-- !FIXME! This would probably be cleaner with lenses
createClippedActions :: RenderActions -> RenderActions -> RenderActions
createClippedActions clip draw =
  RenderActions { renderScene = RenderClipped (rs clip) (rs draw),
                  renderUI = RenderClipped (ru clip) (ru draw) }
  where
    rs = renderScene
    ru = renderUI

addClippedRenderAction :: GameMonad b -> (b -> GameMonad a) -> GameMonad a
addClippedRenderAction clip drawWithClip = GameMonad . RWST $ \cfg input -> do
  -- Get the actions that render the clip
  (clipResult, clipInput, (clipActions, clipRenderActions)) <-
    runRWST (nextFrame clip) cfg input

  -- Get the actions that render our clipped geometry
  (result, finalInput, (finalActions, finalRenderActions)) <-
    runRWST (nextFrame $ drawWithClip clipResult) cfg clipInput

  -- Return with clipped actions
  return (result, finalInput,
          (clipActions ++ finalActions,
           createClippedActions clipRenderActions finalRenderActions))

createTransformedActions :: Transform -> RenderActions -> RenderActions
createTransformedActions xf new =
  RenderActions { renderScene = RenderTransformed xf (renderScene new),
                  renderUI = RenderTransformed xf (renderUI new) }

addTransformedRenderAction :: Transform -> GameMonad a -> GameMonad a
addTransformedRenderAction xf = censor $ second $ createTransformedActions xf

addRenderAction :: Transform -> RenderObject -> GameMonad ()
addRenderAction xf ro = GameMonad $
  tell $ ([], RenderActions (RenderTransformed xf $ RenderObjects [ro]) mempty)

addRenderUIAction :: V2 Float -> RenderObject -> GameMonad ()
addRenderUIAction (V2 x y) ro = GameMonad $
  tell $ ([], RenderActions mempty (RenderTransformed xf $ RenderObjects [ro]))
  where
    xf = translate (V3 x y (-1)) identity

nullRenderer :: Renderer
nullRenderer = Renderer
  { mkTexture = \ _ _ _ -> return $ error "null texture!"
  , updateTexture = \_ _ _ _ -> return ()
  , mkDepthTexture = \_ -> return $ error "null depth texture!"
  , createRenderObject = \_ _ -> return $ error "null render object!"
  , render = \_ _ _ -> return ()
  }

openGLRenderer :: GLFW.Window -> Renderer
openGLRenderer win = Renderer
  { mkTexture = OpenGL.initializeTexture
  , updateTexture = OpenGL.updateTexture
  , mkDepthTexture = OpenGL.createDepthTexture
  , createRenderObject = OpenGL.createRenderObject
  , render = OpenGL.render win
  } 
