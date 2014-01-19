module Graphics.UI.Lambency (
  Input(..),
  makeWindow,
  destroyWindow,
  run
) where

--------------------------------------------------------------------------------

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Graphics.Rendering.Lambency
import Graphics.Rendering.Lambency.Types

import Graphics.UI.Lambency.Input

import Control.Monad.RWS.Strict
import qualified Control.Wire as W

import qualified Data.Set as Set
import qualified Data.Map as Map

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
    m <- GLFW.createWindow width height title Nothing Nothing
    if m == (Nothing) then ioError (userError "Failed!") else do
      putStrLn "Done."
      GLFW.makeContextCurrent m

      -- Initial defaults
      GL.depthFunc GL.$= Just GL.Lequal
      GL.cullFace GL.$= Just GL.Back
      initLambency
      return m

destroyWindow :: Maybe GLFW.Window -> IO ()
destroyWindow m = do
  case m of
    (Just win) -> do
      GLFW.destroyWindow win
    Nothing -> return ()
  GLFW.terminate  

run :: GLFW.Window -> Game -> IO ()
run win g = do
  GLFW.swapInterval 1
  ctl <- mkInputControl win
  let session = W.countSession 0.05
  run' ctl session (staticGameState g, g)
  where

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
        toRenderObjs :: [GameObject] -> [RenderObject]
        toRenderObjs = (>>= toRenderObj)
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

    step :: Game -> Timestep -> GameMonad (GameState, Game)
    step game ts = do
      (Right cam, nCamWire) <- W.stepWire (mainCamera game) ts (Right ())
      gameObjs <- mapM (\w -> W.stepWire w ts $ Right ()) (gameObjects game)
      lightObjs <- mapM (\w -> W.stepWire w ts $ Right ()) (dynamicLights game)
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

    mergeState :: GameState -> GameState -> GameState
    mergeState (_, slights, sobjs) (cam, dlights, dobjs) =
      (cam, slights ++ dlights, sobjs ++ dobjs)

    doOutput :: [LogAction] -> IO ()
    doOutput [] = return ()
    doOutput (StringOutput s : la) = do
      print s
      doOutput la

    run' :: InputControl -> GameSession -> (GameState, Game) -> IO ()
    run' ctl session (st, game) = do

      -- Poll events...
      GLFW.pollEvents

      input <- getInput ctl
      if Set.member GLFW.Key'Q (keysPressed input)
        then GLFW.setWindowShouldClose win True
        else return ()

      -- Step
      (timestep, nextsession) <- W.stepSession session
      let wholeState = mergeState (staticGameState game) st
          ((nextState, nextWires), newIpt, actions) =
            runRWS (step game timestep) wholeState input

      -- Anything happen?
      doOutput actions

      -- Render
      renderState $ mergeState (staticGameState game) nextState

      -- Reset the input
      setInput ctl newIpt

      -- Check for exit
      q <- GLFW.windowShouldClose win
      unless q $ run' ctl nextsession (nextState, nextWires)
