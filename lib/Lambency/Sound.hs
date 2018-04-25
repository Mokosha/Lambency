module Lambency.Sound (
  Sound, SoundCommand(..),
  initSound,
  freeSound,
  loadSound,
  handleCommand,
  startSound, stopSound
) where

--------------------------------------------------------------------------------
import qualified Codec.Wav as Wav
import Control.Monad.RWS.Strict

import Data.Array.Storable
import Data.Audio

import GHC.Int

import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Types

import qualified Sound.OpenAL.AL as AL
import qualified Sound.OpenAL.ALC as ALC
--------------------------------------------------------------------------------

initSound :: IO ()
initSound = do
  device <- ALC.openDevice Nothing
  case device of
    Nothing -> error "Failed to find audio device"
    Just d -> do
      context <- ALC.createContext d []
      ALC.currentContext GL.$= context
      case context of
        Nothing -> putStrLn "WARNING: Failed to set current audio context"
        Just _ -> putStrLn "Audio context successfully initiated."

      -- Setup source and listener...
      AL.listenerPosition GL.$= (GL.Vertex3 0 0 0)
      AL.listenerVelocity GL.$= (GL.Vector3 0 0 0)
      AL.orientation GL.$= (GL.Vector3 0 0 (-1), GL.Vector3 0 1 0)

loadSound :: FilePath -> ResourceLoader Sound
loadSound fp = do
  -- Generate OpenAL source
  (source, buffer) <- liftIO $ do
    src <- GL.genObjectName

    AL.pitch src GL.$= 1
    AL.sourceGain src GL.$= 1
    AL.sourcePosition src GL.$= (GL.Vertex3 0 0 0)
    AL.sourceVelocity src GL.$= (GL.Vector3 0 0 0)
    AL.loopingMode src GL.$= AL.OneShot

    -- Load wav file
    result <- (Wav.importFile :: FilePath -> IO (Either String (Audio Int16))) fp
    a <- case result of
      Left s -> error s
      Right audio -> return audio
    samples <- thaw (sampleData a)

    -- Generate OpenAL buffer
    buf <- GL.genObjectName
    withStorableArray samples $ \ptr -> do
      (sidx, eidx) <- getBounds samples
      let nSamples = (fromIntegral eidx) - (fromIntegral sidx) + 1
          nChannels = fromIntegral (channelNumber a)
          -- Stereo means multiple samples per channel
          mem = AL.MemoryRegion ptr (nChannels * nSamples)
          format = if nChannels > 1 then AL.Stereo16 else AL.Mono16
          freq = fromIntegral (sampleRate a)
          soundData = AL.BufferData mem format freq
      AL.bufferData buf GL.$= soundData

    -- Attach the src to the buffer...
    AL.buffer src GL.$= (Just buf)
    putStrLn $ "Loaded sound: " ++ (show src)

    return (src, buf)

  tell $ GL.deleteObjectName source
      >> GL.deleteObjectName buffer
      >> putStrLn ("Unloaded sound: " ++ show source)

  return source

handleCommand :: Sound -> SoundCommand -> IO ()
handleCommand src StartSound = AL.play [src]
handleCommand src StopSound = AL.stop [src]

freeSound :: IO ()
freeSound = do
  context <- GL.get ALC.currentContext
  case context of
    Nothing -> return ()
    Just c -> do
      (Just d) <- GL.get $ ALC.contextsDevice c
      ALC.currentContext GL.$= Nothing
      ALC.destroyContext c
      result <- ALC.closeDevice d
      if result then return () else error "Failed to close device!"

startSound :: Sound -> GameMonad ()
startSound sound = GameMonad $ tell $ ([SoundAction sound StartSound], mempty)

stopSound :: Sound -> GameMonad ()
stopSound sound = GameMonad $ tell $ ([SoundAction sound StopSound], mempty)
