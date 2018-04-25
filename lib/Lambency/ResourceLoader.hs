module Lambency.ResourceLoader (
    runResourceLoader
  , runLoaderWith
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Reader
import Control.Monad.Writer

import Lambency.Types
--------------------------------------------------------------------------------

-- Takes a function that uses a value within a context in order to pass that
-- value to a different resource loader. As an example:
--   Vector.unsafeWith dat $ \ptr -> loadTexture ptr
--       :: IO Texture
--   Vector.unsafeWith dat
--       :: (Ptr -> IO Texture) -> IO Texture
--   loadTextureAsResource
--       :: Ptr -> ResourceLoader Texture
--   runLoaderWith (Vector.unsafeWith dat) loadTextureAsResource
--       :: ResourceLoader Texture
runLoaderWith :: ((a -> IO (b, IO ())) -> IO (b, IO ())) -> (a -> ResourceLoader b)
              -> ResourceLoader b
runLoaderWith ioPrg rlGen = ResourceLoader . ReaderT $ \rr -> do
  (result, unload) <- liftIO $ ioPrg $ \ptr -> do
    let (ResourceLoader rlPrg) = rlGen ptr
    runWriterT (runReaderT rlPrg rr)
  WriterT (return (result, unload))

runResourceLoader :: Renderer -> ResourceLoader a -> IO (a, IO ())
runResourceLoader r (ResourceLoader prg) = runWriterT (runReaderT prg r)
