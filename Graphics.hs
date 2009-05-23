module Graphics
( display
) where

import Architecture
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
       scale (2 :: GLfloat) 2 (-1e-5)

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
display state_ref =
    do state <- readIORef state_ref
       display_universe state
       display_osd state
       swapBuffers

display_universe :: State -> IO ()
display_universe state =
    do clear [ColorBuffer, DepthBuffer]
       depthFunc $= Just Less
       set_projection_matrix (player_pos state) (player_fwd state) (player_up (state_calc state)) (player_right (state_calc state))
       preservingMatrix $ do scale4 1 1 1 (-1::Double)
			     color (Color3 1 1 1 :: Color3 Double)
			     sphere (bottom_sphere_radius)
       -- x pole
       preservingMatrix $ do swap_wx
			     color (Color3 1 0 0 :: Color3 Double)
			     sphere 0.1
       -- y pole
       preservingMatrix $ do swap_wy
			     color (Color3 0 1 0 :: Color3 Double)
			     sphere 0.1
       -- z pole
       preservingMatrix $ do swap_wz
			     color (Color3 0 0 1 :: Color3 Double)
			     sphere 0.1

       sequence_ $ map draw_ft world_arch

display_osd :: State -> IO ()
display_osd state =
    do depthFunc $= Nothing
       matrixMode $= Projection
       loadIdentity
       matrixMode $= Modelview 0
       if on_the_ground state
           then do color (Color3 1 1 0 :: Color3 Double)
                   renderPrimitive Quads (sequence_ (map vertex mostly_q))
           else return ()
    where
        mostly_q, really_q :: [Vertex3 GLdouble]
        mostly_q = [Vertex3 0.1 0.1 0, Vertex3 0.1 0.2 0, Vertex3 0.2 0.2 0, Vertex3 0.2 0.1 0]
        really_q = [Vertex3 0.3 0.1 0, Vertex3 0.3 0.2 0, Vertex3 0.4 0.2 0, Vertex3 0.4 0.1 0]

swap_wx :: IO ()
swap_wx = (newMatrix ColumnMajor [0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0] :: IO (GLmatrix Double)) >>= multMatrix

swap_wy :: IO ()
swap_wy = (newMatrix ColumnMajor [1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0] :: IO (GLmatrix Double)) >>= multMatrix

swap_wz :: IO ()
swap_wz = (newMatrix ColumnMajor [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0] :: IO (GLmatrix Double)) >>= multMatrix

scale4 :: (MatrixComponent c, Num c) => c -> c -> c -> c -> IO ()
scale4 x y z w = newGLMatrix ColumnMajor [x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, w] >>= multMatrix

newGLMatrix :: (MatrixComponent c) => MatrixOrder -> [c] -> IO (GLmatrix c)
newGLMatrix = newMatrix

sphere :: GLdouble -> IO ()
sphere radius = -- radius is the radius in radians
		-- A Euclidean sphere about the origin is a spherical sphere
                -- about the w pole, so we just let GLU do this for us.
                let euclid_radius = tan radius in
                renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere euclid_radius 100 100)
