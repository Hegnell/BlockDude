module Main (
 main
) where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

import Levels
import Game


data Action = Action (IO Action)

data PlayerState = Playing | End

data Game = Game {
  gameState :: GameState
}

data GameState = GameState {
	player :: Game.Player,
	level :: Levels.Level

} deriving Show


checkPlayerState :: Levels.Level -> Player -> PlayerState
checkPlayerState level player =
	if p == 'e'
	  then End
	  else Playing
	where
	  p = Levels.getObject level px py
	  px = x $ pos player
	  py = y $ pos player


initialLevel :: GameState
initialLevel = GameState {
	player = Game.createPlayer l,
	level = l
} where
	l = Levels.levels !! 0



nextLevel :: GameState -> Maybe GameState
nextLevel state =
	if next > length Levels.levels
		then Nothing
		else Just GameState {
			player = Game.createPlayer l,
			level = l
	 }
	where
	  next = (number $ level state) + 1
	  l = Levels.levels !! next

renderLevel :: Levels.Level -> IO ()
renderLevel level = do
  forM_ (zip (Levels.layout level) [0..]) $ \(row, i) ->
    forM_ (zip row [0..]) $ \(c,j) ->
      case c of
        'x' -> putStrLn "test"
        'e' -> putStrLn "lollslsl"
        _ -> return ()


render :: GameState -> IO ()
render state = do
  renderLevel (level state)

square :: IO ()
square = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f 0.2 0.2 0
    vertex3f 0.2 0 0
 
    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f 0.2 (-0.2) 0
    vertex3f 0.2 0 0
 
    color3f 0 0 1
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f (-0.2) (-0.2) 0
    vertex3f (-0.2) 0 0
 
    color3f 1 0 1
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f (-0.2) 0.2 0
    vertex3f (-0.2) 0 0
  flush


main = do
  -- invoke either active or passive drawing loop depending on command line argument
  args <- getArgs
  prog <- getProgName
  case args of
    ["active"]  -> putStrLn "Running in active mode" >> main' active
    _ -> putStrLn $ "USAGE: " ++ prog ++ " [active|passive]"

main' run = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GLU.ortho2D 0 (realToFrac w) (realToFrac h) 0
  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  -- run the main loop
  let state = initialLevel	
  renderLevel (level state)
  run lines
  -- finish up
  GLFW.closeWindow
  GLFW.terminate


-- we start with waitForPress action
active lines = loop waitForPress
  where 
    loop action = do
      -- draw the entire screen
      --let state = initialLevel	
      render initialLevel
      -- swap buffer
      GLFW.swapBuffers
      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
            -- perform action
            Action action' <- action
            -- sleep for 1ms to yield CPU to other applications
            GLFW.sleep 0.001
 
            -- only continue when the window is not closed
            windowOpen <- getParam Opened
            unless (not windowOpen) $
              loop action' -- loop with next action
 
    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          modifyIORef lines (((x,y):) . ((x,y):))
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        modifyIORef lines (((x,y):) . tail)
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)




--render lines = do
--  l <- readIORef lines
--  GL.clear [GL.ColorBuffer]
--  GL.color $ color3 1 0 0
--  GL.renderPrimitive GL.Lines $ mapM_
--      (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
 
 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3                          