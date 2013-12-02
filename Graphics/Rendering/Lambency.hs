module Graphics.Rendering.Lambency (
  initLambency,
  module Graphics.Rendering.Lambency.Camera,
  module Graphics.Rendering.Lambency.Material,
  module Graphics.Rendering.Lambency.Mesh,
  module Graphics.Rendering.Lambency.Object,
  module Graphics.Rendering.Lambency.Renderable,
  module Graphics.Rendering.Lambency.Shader,
  module Graphics.Rendering.Lambency.Texture,
  module Graphics.Rendering.Lambency.Transform,
  module Graphics.Rendering.Lambency.Utils,
) where

--------------------------------------------------------------------------------

import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency.Camera
import Graphics.Rendering.Lambency.Material
import Graphics.Rendering.Lambency.Mesh
import Graphics.Rendering.Lambency.Object
import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Shader
import Graphics.Rendering.Lambency.Texture
import Graphics.Rendering.Lambency.Transform
import Graphics.Rendering.Lambency.Utils
--------------------------------------------------------------------------------

initLambency :: IO ()
initLambency = do
  putStrLn "Initializing..."
  printInfo GL.vendor "Vendor: "
  printInfo GL.renderer "Renderer: "
  printInfo GL.glVersion "OpenGL Version: "
  printInfo GL.shadingLanguageVersion "GLSL Version: "
  -- (mapM_ putStrLn) =<< (GL.get GL.glExtensions)
  putStrLn "Done initializing..."
  where
    printInfo :: (GL.GettableStateVar String) -> String -> IO ()
    printInfo sv s = (=<<) (putStrLn . ((++) s)) $ GL.get sv
