module Main (main) where

import Constants
import Data.IORef
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import Math
import System

data State = State { player_pos, player_fwd :: Vec4
                   }

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

initial_state :: State
--initial_state = State (Math.normalize (V4 (tan (bottom_sphere_radius + player_height)) 0 0 1)) (V4 0 0 1 0)
initial_state = State (V4 0 1 0 0) (V4 0 0 1 0)

set_projection_matrix :: Vec4 -> Vec4 -> IO ()
set_projection_matrix pos fwd =
    -- The camera is sitting at the z pole looking toward the w pole.
    do matrixMode $= Projection
       loadIdentity
       scale (0.5 :: GLfloat) 0.5 1e-5

       -- At this point, the camera is sitting at the z pole looking toward the
       -- w pole.  Above the camera is the y pole, and to the right is the x
       -- pole.  We now change these facts so that the camera is sitting at pos looking
       -- in the direction fwd.  Down is toward the -w pole.  fwd must be
       -- orthogonal to -w and to pos.
       
       let up' = V4 0 0 0 1
       let right = Math.normalize (cross4 pos fwd up')
       let up = cross4 pos right fwd
       --(newMatrix RowMajor ([right, up, pos, fwd] >>= coords) :: IO (GLmatrix Double)) >>= multMatrix
       (newMatrix RowMajor ([V4 1 0 0 0, V4 0 0 0 1, V4 0 1 0 0, V4 0 0 1 0] >>= coords) :: IO (GLmatrix Double)) >>= multMatrix

       matrixMode $= Modelview 0
    where
        coords (V4 x y z w) = [x, y, z, w]

display :: IORef State -> IO ()
display state_ref = do state <- readIORef state_ref
                       clear [ColorBuffer, DepthBuffer]
                       set_projection_matrix (player_pos state) (player_fwd state)
                       color (Color3 1 1 1 :: Color3 Double)
                       --sphere bottom_sphere_radius
                       preservingMatrix $ do swap_wz
                                             color (Color3 1 0 0 :: Color3 Double)
                                             sphere 0.3
                       preservingMatrix $ do rotate (45::Double) (Vector3 1 0 0)
                                             swap_wz
                                             color (Color3 0 0 1 :: Color3 Double)
                                             sphere 0.15
                       preservingMatrix $ do swap_wz
                                             color (Color3 0 1 0 :: Color3 Double)
                                             sphere 0.15
                       swapBuffers

swap_wz :: IO ()
swap_wz = (newMatrix ColumnMajor [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0] :: IO (GLmatrix Double)) >>= multMatrix

sphere :: GLdouble -> IO ()
sphere radius = -- radius is the radius in radians
		-- A Euclidean sphere about the origin is a spherical sphere
                -- about the w pole, so we just let GLU do this for us.
                let euclid_radius = tan radius in
                renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere euclid_radius 100 100)

update :: State -> State
update = id

fix :: (a -> a) -> a
fix f = let x = f x in x
