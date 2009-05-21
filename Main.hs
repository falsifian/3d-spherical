module Main (main) where

import Data.IORef
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import System

data State = State ()

main :: IO ()
main = do args_after_glut <- getArgs >>= initialize "Some Program" -- TODO: program name
          initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
          -- TODO: Do I need this? -- depthFunc $= Just Less
          initialWindowSize $= Size 640 640 -- TODO: size
          window <- createWindow "GLUT Window" -- TODO: window name
          state_ref <- newIORef (State ())
          displayCallback $= display state_ref
          fix (\f -> modifyIORef state_ref update >> postRedisplay Nothing >> addTimerCallback 50 f)
          mainLoop

display :: IORef State -> IO ()
display state_ref = do clear [ColorBuffer, DepthBuffer]
                       matrixMode $= Projection
                       loadIdentity
                       scale (0.5 :: GLfloat) 0.5 1e-5
                       matrixMode $= Modelview 0
                       materialAmbient FrontAndBack $= Color4 1 1 1 1
                       sphere 0.1
                       swapBuffers

sphere :: GLdouble -> IO ()
sphere radius = -- radius is the radius in radians
		-- A Euclidean sphere about the origin is a spherical sphere
                -- about the w pole, so we just let GLU do this for us.
                let euclid_radius = sin radius in
                renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere euclid_radius 10 10)

update :: State -> State
update = id

fix :: (a -> a) -> a
fix f = let x = f x in x
