module Graphics.Rendering.Lambency.Mesh (
  makeTriangle
) where

--------------------------------------------------------------------------------

import Graphics.Rendering.Lambency.Renderable
import Graphics.Rendering.Lambency.Vertex
import Data.Vect.Float
import Control.Applicative

import qualified Graphics.Rendering.OpenGL as GL
import Foreign.Ptr


--------------------------------------------------------------------------------

data Mesh = Mesh { vertices :: [Vec3] }
-----          | NMesh { vertices :: [Vec3], normals :: [Vec3] } deriving (Show)

makeTriangle :: Mesh
makeTriangle = Mesh{vertices = mkVec3 <$> [ (-1, -1, 0), (1, -1, 0), (0, 1, 0)]}

renderMesh :: RenderObject -> IO ()
renderMesh ro = let
  nv = nVerts ro
  vloc = GL.AttribLocation 0
  vadesc = GL.VertexArrayDescriptor 3 GL.Float 0 (nullPtr :: Ptr Float)
  in
   do
     -- Bind appropriate buffers
     GL.bindBuffer GL.ArrayBuffer GL.$= (Just $ vertexBufferObject ro)
     GL.vertexAttribPointer vloc GL.$= (GL.ToFloat, vadesc)

     -- Render
     GL.drawArrays GL.Triangles 0 nv

instance Renderable Mesh where
  createRenderObject m = do
    ro <- createROWithVertices (map mkVertex3 $ vertices m) renderMesh
    return ro

{--
  let
    SimpleMaterial{ shaderProgram = prog,
                    beforeRender = br,
                    afterRender = ar }  in
   do
     -- Allocate memory for float list for MVP matrix...
     case (shaderProgram ro) of
       Nothing -> return ()
       Just prg -> do
         (GL.UniformLocation mvpLoc) <- GL.get $ GL.uniformLocation prg "mvpMatrix"
         mvpArr <- newListArray (0 :: Int, 15) (map realToFrac $ getViewProjMatrix c)
         withStorableArray mvpArr (\ptr -> GLRaw.glUniformMatrix4fv mvpLoc 1 0 ptr)
--}



