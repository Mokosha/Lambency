module Graphics.Rendering.Lambency (
  initLambency,
  module Graphics.Rendering.Lambency.Renderable,
  module Graphics.Rendering.Lambency.Shader,
  module Graphics.Rendering.Lambency.Camera,
  module Graphics.Rendering.Lambency.Mesh
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Mesh
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Camera
--------------------------------------------------------------------------------

initLambency :: IO ()
initLambency = do
  putStrLn "Initializing..."
  printInfo GL.vendor "Vendor: "
  printInfo GL.renderer "Renderer: "
  printInfo GL.glVersion "OpenGL Version: "
  printInfo GL.shadingLanguageVersion "GLSL Version: "
  -- (mapM_ putStrLn) =<< (GL.get GL.glExtensions)
  vaos <- GL.genObjectNames 1
  case vaos of
    vao : _ -> do
      let vaoBind = GL.bindVertexArrayObject
        in
       vaoBind GL.$= (Just vao)
    _ -> return ()
  putStrLn "Done initializing..."
  where
    printInfo :: (GL.GettableStateVar String) -> String -> IO ()
    printInfo sv s = (=<<) (putStrLn . ((++) s)) $ GL.get sv
