module Main (main) where

import Constants
import Data.IORef
import qualified Data.Set as Set
import Engine
import Graphics
import Graphics.UI.GLUT
import System

main :: IO ()
main = do args_after_glut <- getArgs >>= initialize program_name
          initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
          initialWindowSize $= window_size
          window <- createWindow window_title
          state_ref <- newIORef initial_state
          displayCallback $= display state_ref
          keyboardMouseCallback $= Just (keyboard_mouse_callback state_ref)
          initDisplay
          fix (\f -> modifyIORef state_ref update >> postRedisplay Nothing >> addTimerCallback 50 f)
          mainLoop

keyboard_mouse_callback :: IORef State -> KeyboardMouseCallback
keyboard_mouse_callback state_ref (Char c) key_state _ _ =
    case kmap c of
        Just k -> modifyIORef state_ref (\state -> state { keys = (case key_state of Down -> Set.insert; Up -> Set.delete) k (keys state) })
        Nothing -> return ()
keyboard_mouse_callback _ (SpecialKey c) _ _ _ = return ()
keyboard_mouse_callback _ (MouseButton c) _ _ _ = return ()

kmap :: Char -> Maybe Engine.Key
kmap ' ' = Just KJump
kmap '4' = Just KLeft
kmap '5' = Just KBwd
kmap '6' = Just KRight
kmap '8' = Just KFwd
kmap '0' = Just KJump
kmap '7' = Just KDown
kmap '9' = Just KUp
kmap '+' = Just KHover
kmap _ = Nothing

fix :: (a -> a) -> a
fix f = let x = f x in x
