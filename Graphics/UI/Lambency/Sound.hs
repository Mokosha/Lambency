module Graphics.UI.Lambency.Sound (
  Sound, SoundCtl, SoundCommand(..),
  initSound,
  freeSound,
  createSoundCtl,
  loadSound,
  handleCommand,
) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM

-- import qualified Graphics.UI.SDL as SDL
-- import qualified Graphics.UI.SDL.Mixer as Mix

import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- type Sound = Mix.Chunk
type Sound = ()
-- type SoundCtl = TVar (Map.Map Sound Mix.Channel)
type SoundCtl = TVar (Map.Map Sound ())
data SoundCommand = StartSound
                  | StopSound

createSoundCtl :: IO (SoundCtl)
createSoundCtl = newTVarIO $ Map.empty

initSound :: IO ()
initSound = do
  return ()
--  SDL.init [SDL.InitAudio]
--  Mix.openAudio audioRate audioFormat audioChannels audioBuffers
--  where audioRate     = 22050
--        audioFormat   = Mix.AudioS16LSB
--        audioChannels = 256
--        audioBuffers  = 4096

loadSound :: FilePath -> IO (Sound)
-- loadSound fp = return =<< Mix.loadWAV fp
loadSound fp = return ()

handleCommand :: SoundCtl -> Sound -> SoundCommand -> IO ()
--handleCommand ctl sound StartSound = do
--  smap <- readTVarIO ctl
--  nmap <- do
--    ch <- Mix.playChannel (-1) sound 0
--    return $ Map.insert sound ch smap
--  atomically $ writeTVar ctl nmap
handleCommand _ _ StartSound = return ()

--handleCommand ctl sound StopSound = do
--  smap <- readTVarIO ctl
--  nmap <- do
--    case (Map.lookup sound smap) of
--      Nothing -> return smap
--      Just ch -> do
--        Mix.haltChannel ch
--        return $ Map.delete sound smap
--  atomically $ writeTVar ctl nmap
handleCommand _ _ StartSound = return ()

freeSound :: IO ()
--freeSound = do
--  Mix.closeAudio
--  SDL.quit
freeSound = return ()
