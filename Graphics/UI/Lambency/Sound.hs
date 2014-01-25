module Graphics.UI.Lambency.Sound (
  Sound, SoundObject, SoundCtl,
  initSound,
  freeSound,
  createSoundCtl,
  loadSound,
  handleCommand,
  startSound,
  stopSound
) where

--------------------------------------------------------------------------------

import Control.Concurrent.STM

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Mixer as Mix

import qualified Data.Map as Map

--------------------------------------------------------------------------------

type Sound = Mix.Chunk
type SoundCtl = TVar (Map.Map Sound Mix.Channel)

createSoundCtl :: IO (SoundCtl)
createSoundCtl = newTVarIO $ Map.empty

initSound :: IO ()
initSound = do
  SDL.init [SDL.InitAudio]
  Mix.openAudio audioRate audioFormat audioChannels audioBuffers
  where audioRate     = 22050
        audioFormat   = Mix.AudioS16LSB
        audioChannels = 256
        audioBuffers  = 4096

loadSound :: FilePath -> IO (SoundObject)
loadSound fp = do
  s <- Mix.loadWAV fp
  return $ SoundObject { sound = s, command = Nothing }

handleCommand :: SoundCtl -> SoundObject -> IO ()
handleCommand ctl so = do
  smap <- readTVarIO ctl
  nmap <-
    case (command so) of
      Nothing -> return smap
      Just StartSound -> do
        let s = (sound so)
        ch <- Mix.playChannel (-1) s 0
        return $ Map.insert s ch smap

      Just StopSound ->
        case (Map.lookup (sound so) smap) of
          Nothing -> return smap
          Just ch -> do
            Mix.haltChannel ch
            return $ Map.delete (sound so) smap

  atomically $ writeTVar ctl nmap

freeSound :: IO ()
freeSound = do
  Mix.closeAudio
  SDL.quit

--------------------------------------------------------------------------------

-- Audio Objects

data SoundObject = SoundObject {
  sound :: Sound,
  command :: Maybe SoundCommand
}

data SoundCommand = StartSound
                  | StopSound

startSound :: SoundObject -> SoundObject
startSound = (\so -> so { command = Just StartSound })

stopSound :: SoundObject -> SoundObject
stopSound = (\so -> so { command = Just StopSound })
