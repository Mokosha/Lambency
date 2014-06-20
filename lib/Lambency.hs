module Lambency (
  initLambency,
  module Lambency.Bounds,
  module Lambency.Camera,
  module Lambency.GameObject,
  module Lambency.Light,
  module Lambency.Loaders,  
  module Lambency.Material,
  Mesh, triangle, cube, plane, quad,
  module Lambency.Renderable,
  module Lambency.Shader,
  module Lambency.Texture,
  module Lambency.Transform,
  Camera, CameraType, CameraViewDistance,
  LightType, Light,
  ShaderValue(..), ShaderMap,
  Material,
  RenderFlag(..), RenderObject(..),
  OutputAction(..),
  TimeStep,
  Game(..), GameWire, GameState, GameMonad,
  module Lambency.Utils,

  Input(..), withPressedKey, isKeyPressed, debounceKey,
  isButtonPressed,
  resetCursorPos,
  makeWindow, destroyWindow, run,
  module Lambency.Sound
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Lambency.Bounds
import Lambency.Camera
import Lambency.GameObject
import Lambency.Input
import Lambency.Light
import Lambency.Loaders
import Lambency.Material
import Lambency.Mesh
import Lambency.Renderable
import Lambency.Shader
import Lambency.Sound
import Lambency.Texture
import Lambency.Transform
import Lambency.Types
import Lambency.Utils

import Control.Monad.RWS.Strict
import qualified Control.Wire as W

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Time
import Data.List (sortBy, partition)

import GHC.Float

import Linear.Matrix
import Linear.V4

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
      GL.blend GL.$= GL.Enabled
      GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
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
type StateStepper a = (Game a, Input)

run :: GLFW.Window -> a -> Game a -> IO ()
run win initialGameObject initialGame = do
  GLFW.swapInterval 1
  ictl <- mkInputControl win
  let session = W.countSession (double2Float physicsDeltaTime) W.<*> W.pure ()
  curTime <- getCurrentTime
  run' ictl
    initialGameObject
    session
    (curTime, diffUTCTime curTime curTime)
    initialGame
  where

    place :: Transform -> Camera -> RenderObject -> RenderObject
    place xf cam ro = let
      model :: M44 Float
      model = xform2Matrix xf

      sm :: ShaderMap
      sm = Map.fromList [
        ("mvpMatrix", Matrix4Val $ model !*! (getViewProjMatrix cam)),
        ("m2wMatrix", Matrix4Val $ model)]
      in
       ro { material = Map.union sm (material ro) }

    step :: a -> Game a -> TimeStep -> GameMonad (Either () a, Camera, [Light], Game a)
    step go game t = do
      (Right cam, nCamWire) <- W.stepWire (mainCamera game) t (Right ())
      (result, gameWire) <- W.stepWire (gameLogic game) t (Right go)
      lightObjs <- mapM (\w -> W.stepWire w t $ Right ()) (dynamicLights game)
      let (lights, lwires) = collect lightObjs
      return (result, cam, lights ++ (staticLights game), newGame nCamWire lwires gameWire)
        where
          collect :: [(Either e b, GameWire a b)] -> ([b], [GameWire a b])
          collect [] = ([], [])
          collect ((Left _, _) : rest) = collect rest
          collect ((Right obj, wire) : rest) = let
            (objs, wires) = collect rest
            in
             (obj : objs, wire : wires)

          newGame cam lights logic = Game {
            staticLights = (staticLights game),
            staticGeometry = (staticGeometry game),
            mainCamera = cam,
            dynamicLights = lights,
            gameLogic = logic}

    performAction :: (OutputAction -> IO(Maybe OutputAction)) ->
                     [OutputAction] -> IO ([OutputAction])
    performAction _ [] = return []
    performAction fn (act : acts) = do
      res <- fn act
      case res of
        Nothing -> performAction fn acts
        Just a -> do
          ress <- performAction fn acts
          return $ a : ress

    playSounds :: [OutputAction] -> IO ([OutputAction])
    playSounds = performAction $
                 (\act -> case act of
                     SoundAction sound cmd -> do
                       handleCommand sound cmd
                       return Nothing
                     _ -> do
                       return (Just act))

    printLogs :: [OutputAction] -> IO ([OutputAction])
    printLogs [] = return []
    printLogs (LogAction s : rest) = do
      print s
      printLogs rest
    printLogs (act : acts) = do
      rest <- printLogs acts
      return (act : rest)

    renderObjects :: [Light] -> Camera -> [OutputAction] -> IO ([OutputAction])
                  -- This is the best line in my code
    renderObjects lights camera action = let
      camDist :: RenderObject -> Float
      camDist ro =
        let (Matrix4Val (V4 _ _ (V4 _ _ _ z) _)) = getMaterialVar (material ro) "mvpMatrix" in z

      (trans, opaque) = partition (\ro -> Transparent `elem` (flags ro)) $
                        sortBy (\ro1 ro2 -> compare (camDist ro1) (camDist ro2)) $
                        do
                          act <- action
                          (xf, ro) <- case act of
                            Render3DAction xf' ro' -> [(xf', ro')]
                            _ -> []
                          return $ place xf camera ro

      render' ros
        | length ros == 0 = return ()
        | otherwise = do
          mapM_ (flip renderLight ros) lights
      in do
        -- !FIXME! This should be moved to the camera...
        GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
        clearBuffers

        GL.depthFunc GL.$= Just GL.Lequal
        render' opaque
        GL.depthFunc GL.$= Nothing
        render' (reverse trans)

        GL.flush
        GLFW.swapBuffers win

        return $ filter (\act -> case act of
                          Render3DAction _ _ -> False
                          _ -> True) action

    run' :: InputControl ->
            a -> GameSession -> GameTime -> Game a ->
            IO ()
    run' ictl gameObject session (lastFrameTime, accumulator) game = do

      -- Poll events...
      GLFW.pollEvents

      input <- getInput ictl
      if Set.member GLFW.Key'Q (keysPressed input)
        then GLFW.setWindowShouldClose win True
        else return ()

      -- Step
      curTime <- getCurrentTime
      let newAccum = accumulator + (diffUTCTime curTime lastFrameTime)
      (go, (nextsession, accum), (nextGame, newIpt)) <-
        stepGame gameObject (session, newAccum) (game, input)

      -- Reset the input
      setInput ictl newIpt

      case go of
        Right gobj -> do
          -- Check for exit
          q <- GLFW.windowShouldClose win
          unless q $ run' ictl gobj nextsession (curTime, accum) nextGame
        Left _ -> return ()
     where
       buildRO :: (Transform, RenderObject) -> OutputAction
       buildRO = uncurry Render3DAction

       stepGame :: a -> TimeStepper -> StateStepper a ->
                   IO (Either () a, TimeStepper, StateStepper a)
       stepGame go tstep@(sess, accum) sstep@(g, ipt)
         | accum < physicsDeltaUTC = return (Right go, tstep, sstep)
         | otherwise = do
           (ts, nextSess) <- W.stepSession sess
           let ((result, cam, lights, nextGame), newIpt, actions) =
                 runRWS (step go g ts) () ipt
           if (accum - physicsDeltaUTC) < physicsDeltaUTC then
             -- Anything happen? Do rendering first, then rest of the actions
             foldM_ (\acts f -> f acts) actions [
               (renderObjects lights cam) . ((map buildRO $ staticGeometry g) ++),
               printLogs,
               playSounds]
             else do
             _ <- printLogs actions
             return ()

           case result of
             Right obj -> stepGame obj
                          (nextSess, (accum - physicsDeltaUTC)) -- TimeStepper
                          (nextGame, newIpt)                    -- StateStepper
             Left _ -> return (result, tstep, sstep)
