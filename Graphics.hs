module Graphics
( display
) where

import Constants
import Data.IORef
import Engine
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import Math

set_projection_matrix :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> IO ()
set_projection_matrix pos fwd up right =
    -- The camera is sitting at the z pole looking toward the w pole.
    do matrixMode $= Projection
       loadIdentity
       scale (0.5 :: GLfloat) 0.5 1e-5

       -- At this point, the camera is sitting at the z pole looking toward the
       -- w pole.  Above the camera is the y pole, and to the right is the x
       -- pole.  We now change these facts so that the camera is sitting at pos looking
       -- in the direction fwd.  Down is toward the -w pole.  fwd must be
       -- orthogonal to -w and to pos.
       
       (newMatrix RowMajor ([right, up, pos, fwd] >>= coords) :: IO (GLmatrix Double)) >>= multMatrix

       matrixMode $= Modelview 0
    where
        coords (V4 x y z w) = [x, y, z, w]

display :: IORef State -> IO ()
display state_ref = do state <- readIORef state_ref
                       clear [ColorBuffer, DepthBuffer]
                       set_projection_matrix (player_pos state) (player_fwd state) (player_up (state_calc state)) (player_right (state_calc state))
                       preservingMatrix $ do color (Color3 1 1 1 :: Color3 Double)
                                             sphere bottom_sphere_radius
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