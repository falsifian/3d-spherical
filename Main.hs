module Main (main) where

import Constants
import Data.IORef
import Engine
import Graphics
import Graphics.UI.GLUT
import System

main :: IO ()
main = do args_after_glut <- getArgs >>= initialize program_name
          initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
          initialWindowSize $= window_size
          window <- createWindow window_title
          depthFunc $= Just Less
          state_ref <- newIORef initial_state
          displayCallback $= display state_ref
          fix (\f -> modifyIORef state_ref update >> postRedisplay Nothing >> addTimerCallback 50 f)
          mainLoop

fix :: (a -> a) -> a
fix f = let x = f x in x
