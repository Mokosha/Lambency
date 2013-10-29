module Graphics.Rendering.Lambency (
  initLambency,
  
  RenderObject,
  createRenderObject,
  
  Renderable,
  render,
  
  makeTriangle,

  Shader,
  loadShader
  ) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Mesh
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader

import Control.Applicative
--------------------------------------------------------------------------------

initLambency :: IO ()
initLambency = do
  putStrLn "Initializing..."
  (=<<) putStrLn $ GL.get GL.vendor
  (=<<) putStrLn $ GL.get GL.renderer
  (=<<) putStrLn $ GL.get GL.glVersion
  (=<<) (putStrLn . show) $ GL.get GL.glExtensions
  (=<<) putStrLn $ GL.get GL.shadingLanguageVersion
  vaos <- GL.genObjectNames 1
  case vaos of
    vao : _ -> do
      let vaoBind = GL.bindVertexArrayObject
        in
       vaoBind GL.$= (Just vao)
    _ -> return ()
  putStrLn "Done initializing..."
