module Graphics.UI.Lambency (
  Input(..),
  makeWindow,
  destroyWindow,
  run,
  module Graphics.UI.Lambency.Sound
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency
import Graphics.Rendering.Lambency.Types

import Graphics.UI.Lambency.Input
import Graphics.UI.Lambency.Sound

import Control.Monad.RWS.Strict
import qualified Control.Wire as W

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Time

import GHC.Float

--------------------------------------------------------------------------------

errorCallback :: GLFW.Error -> String -> IO()
errorCallback e s = putStrLn $ "GLFW Error: " ++ (show e ++ s)

makeWindow :: Int -> Int -> String -> IO (Maybe GLFW.Window)
makeWindow width height title = do
  putStr "Initializing GLFW..."
  r <- GLFW.init
  if not r then ioError (userError "Failed!") else do
    putStrLn "Done"
    GLFW.setErrorCallback $ Just errorCallback
    putStr $ "Creating window of size (" ++ (show width) ++ ", " ++ (show height) ++ ")..."
    GLFW.windowHint $ GLFW.WindowHint'Samples 4
    m <- GLFW.createWindow width height title Nothing Nothing
    if m == (Nothing) then ioError (userError "Failed!") else do
      putStrLn "Done."
      GLFW.makeContextCurrent m

      -- Initial defaults
      GL.depthFunc GL.$= Just GL.Lequal
      GL.cullFace GL.$= Just GL.Back
      initLambency
      initSound
      GL.dither GL.$= GL.Disabled
      return m

destroyWindow :: Maybe GLFW.Window -> IO ()
destroyWindow m = do
  case m of
    (Just win) -> do
      GLFW.destroyWindow win
    Nothing -> return ()
  GLFW.terminate
  freeSound

-- The physics framerate in frames per second
physicsFramerate :: Double
physicsFramerate = 60.0

physicsDeltaTime :: Double
physicsDeltaTime = 1.0 / physicsFramerate

physicsDeltaUTC :: NominalDiffTime
physicsDeltaUTC = let
  someDay :: Day
  someDay = fromGregorian 1970 1 1

  dayStart :: UTCTime
  dayStart = UTCTime {
    utctDay = someDay,
    utctDayTime = secondsToDiffTime 0
    }

  dayEnd :: UTCTime
  dayEnd = UTCTime {
    utctDay = someDay,
    utctDayTime = picosecondsToDiffTime $ round $ 1e12 * physicsDeltaTime
    }
  in
   diffUTCTime dayEnd dayStart

type TimeStepper = (GameSession, NominalDiffTime)
type StateStepper = (GameState, Game, Input)

run :: GLFW.Window -> Game -> IO ()
run win initialGame = do
  GLFW.swapInterval 1
  sctl <- createSoundCtl
  ictl <- mkInputControl win
  let session = W.countSession (double2Float physicsDeltaTime)
  curTime <- getCurrentTime
  run' sctl ictl session
    (curTime, diffUTCTime curTime curTime)
    (staticGameState initialGame, initialGame)
  where

    hearState :: SoundCtl -> GameState -> IO ()
    hearState ctl (_, _, gameObjs) = mapM_ (handleCommand ctl) (toSoundObjs gameObjs)
      where
        collect :: [Component] -> [SoundObject]
        collect [] = []
        collect (SoundComponent so : cs) = so : (collect cs)
        collect (_ : cs) = collect cs
        
        toSoundObj :: GameObject -> [SoundObject]
        toSoundObj (GameObject _ cs) = collect cs
        
        toSoundObjs :: [GameObject] -> [SoundObject]
        toSoundObjs = (>>= toSoundObj)

    renderState :: GameState -> IO ()
    renderState (cam, lights, gameObjs) = do
      -- !FIXME! This should be moved to the camera...
      GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
      clearBuffers
      mapM_ (flip renderLight $ toRenderObjs gameObjs) lights
      GL.flush

      -- Swap buffers and poll events...
      GLFW.swapBuffers win

      where
        collect :: [Component] -> [RenderObject]
        collect [] = []
        collect (RenderComponent ro : cs) = ro : (collect cs)
        collect (_ : cs) = collect cs

        place :: Transform -> RenderObject -> RenderObject
        place xf ro = RenderObject {
          material = Map.union (positioned cam xf) (material ro),
          render = (render ro)
          }

        toRenderObj :: GameObject -> [RenderObject]
        toRenderObj (GameObject xf cs) = map (place xf) (collect cs)

        toRenderObjs :: [GameObject] -> [RenderObject]
        toRenderObjs = (>>= toRenderObj)

    step :: Game -> Timestep -> GameMonad (GameState, Game)
    step game dt = do
      (Right cam, nCamWire) <- W.stepWire (mainCamera game) dt (Right ())
      gameObjs <- mapM (\w -> W.stepWire w dt $ Right ()) (gameObjects game)
      lightObjs <- mapM (\w -> W.stepWire w dt $ Right ()) (dynamicLights game)
      let (objs, wires) = collect gameObjs
          (lights, lwires) = collect lightObjs
      return ((cam, lights, concat objs), newGame nCamWire lwires wires)
        where
          collect :: [(Either e a, GameWire a)] -> ([a], [GameWire a])
          collect [] = ([], [])
          collect ((Left _, _) : rest) = collect rest
          collect ((Right obj, wire) : rest) = let
            (objs, wires) = collect rest
            in
             (obj : objs, wire : wires)

          newGame :: GameWire Camera -> [GameWire Light] -> [GameWire [GameObject]] -> Game
          newGame cam lights objs = Game {
            staticGameState = (staticGameState game),
            mainCamera = cam,
            dynamicLights = lights,
            gameObjects = objs}

    stepGame :: GameState -> TimeStepper -> StateStepper -> [LogAction] ->
                IO (TimeStepper, StateStepper, [LogAction])
    stepGame static tstep@(sess, accum) sstep@(gs, g, ipt) logs
      | accum < physicsDeltaUTC = return (tstep, sstep, logs)
      | otherwise = do
        (ts, nextSess) <- W.stepSession sess
        let wholeState = mergeState static gs
            ((nextState, nextGame), newIpt, actions) =
              runRWS (step g $ ts ()) wholeState ipt
        stepGame
          static                                -- GameState
          (nextSess, (accum - physicsDeltaUTC)) -- TimeStepper
          (nextState, nextGame, newIpt)         -- StateStepper
          (logs ++ actions)                     -- Logs

    mergeState :: GameState -> GameState -> GameState
    mergeState (_, slights, sobjs) (cam, dlights, dobjs) =
      (cam, slights ++ dlights, sobjs ++ dobjs)

    doOutput :: [LogAction] -> IO ()
    doOutput [] = return ()
    doOutput (StringOutput s : la) = do
      print s
      doOutput la

    run' :: SoundCtl ->
            InputControl ->
            GameSession ->
            GameTime ->
            (GameState, Game) ->
            IO ()
    run' sctl ictl session (lastFrameTime, accumulator) (st, game) = do

      -- Poll events...
      GLFW.pollEvents

      input <- getInput ictl
      if Set.member GLFW.Key'Q (keysPressed input)
        then GLFW.setWindowShouldClose win True
        else return ()

      -- Step
      curTime <- getCurrentTime
      let newAccum = accumulator + (diffUTCTime curTime lastFrameTime)
      ((nextsession, accum), (nextState, nextGame, newIpt), actions) <-
        stepGame (staticGameState game) (session, newAccum) (st, game, input) []

      -- Anything happen?
      doOutput actions

      -- Render
      renderState $ mergeState (staticGameState game) nextState

      -- Do audio
      hearState sctl nextState

      -- Reset the input
      setInput ictl newIpt

      -- Check for exit
      q <- GLFW.windowShouldClose win
      unless q $ run' sctl ictl nextsession (curTime, accum) (nextState, nextGame)
