module Graphics.Rendering.Lambency.Loaders.OBJLoader (
  loadOBJ,
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Mesh

--------------------------------------------------------------------------------

loadOBJ :: FilePath -> IO (Mesh)
loadOBJ _ = return triangle
