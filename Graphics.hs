module Graphics
(   initDisplay
,   display
) where

import Architecture hiding (color)
import Constants
import Data.IORef
import Engine
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import Math

data OrthoRect = OR { orMinX, orMaxX, orMinY, orMaxY :: Double }

data OSD =
    OSD { osdRect :: OrthoRect
        , osdBackgroundColour, osdPanelColour :: Color4 Double
        , osdPanels :: [(OrthoRect, Double -> State -> IO ())]
        }

set_matrix :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> IO ()
set_matrix pos fwd up right =
    -- The camera is sitting at the z pole looking toward the w pole.
    do loadIdentity
       scale (1 :: GLfloat) 1 (-1e-5)

       -- At this point, the camera is sitting at the z pole looking toward the
       -- w pole.  Above the camera is the y pole, and to the right is the x
       -- pole.  We now change these facts so that the camera is sitting at pos looking
       -- in the direction fwd.  Down is toward the -w pole.  fwd must be
       -- orthogonal to -w and to pos.
       
       (newMatrix RowMajor ([right, up, pos, fwd] >>= coords) :: IO (GLmatrix Double)) >>= multMatrix
    where
        coords (V4 x y z w) = [x, y, z, w]

initDisplay :: IO ()
initDisplay =
    do -- TODO: Things might go faster if blending is only enabled for the parts that need it.
       blend $= Enabled
       blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

display :: IORef State -> IO ()
display state_ref =
    do state <- readIORef state_ref
       display_universe_twice state
       display_osd defaultOSD state
       swapBuffers

display_universe_twice :: State -> IO ()
display_universe_twice state =
    do depthFunc $= Just Less
       clear [ColorBuffer, DepthBuffer]
       matrixMode $= Projection
       set_matrix (player_pos state) (player_fwd state) (player_up (state_calc state)) (player_right (state_calc state))
       scale4 (-1) (-1) (-1) (-1::Double)
       matrixMode $= Modelview 0
       display_universe_once state
       matrixMode $= Projection
       scale4 (-1) (-1) (-1) (-1::Double)
       matrixMode $= Modelview 0
       clear [DepthBuffer]
       display_universe_once state
       matrixMode $= Projection
       loadIdentity
       matrixMode $= Modelview 0

display_universe_once :: State -> IO ()
display_universe_once state =
    do -- bottom sphere (-w pole)
       preservingMatrix $ do scale4 1 1 1 (-1::Double)
			     color (Color3 1 1 1 :: Color3 Double)
			     sphere (bottom_sphere_radius)
       -- x pole
       preservingMatrix $ do swap_wx
			     color (Color3 1 0 0 :: Color3 Double)
			     sphere 0.3
       -- y pole
       preservingMatrix $ do swap_wy
			     color (Color3 0 1 0 :: Color3 Double)
			     sphere 0.3
       -- z pole
       preservingMatrix $ do swap_wz
			     color (Color3 0 0 1 :: Color3 Double)
			     sphere 0.3

       sequence_ $ map draw_ft world_arch

display_osd :: OSD -> State -> IO ()
display_osd osd state =
    let displayPanel (rect, panel) = preservingMatrix $
            do enterRect rect
               color (osdPanelColour osd)
               fill
               panel (aspect (osdRect osd) * aspect rect) state
    in preservingMatrix $
    do depthFunc $= Nothing
       matrixMode $= Projection
       loadIdentity
       matrixMode $= Modelview 0
       enterRect (osdRect osd)
       color (osdBackgroundColour osd)
       fill
       sequence_ (map displayPanel (osdPanels osd))

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

defaultOSD :: OSD
defaultOSD =
    OSD { osdRect = OR (-1) 1 (-1) (-1/2)
        , osdBackgroundColour = Color4 0 0 0 0.5
        , osdPanelColour = osdBackgroundColour defaultOSD
        , osdPanels =
            -- TODO: use aspect ratio
            [ (OR (-7/8) (-5/8) (-1/2) (1/2), jumpPanel)
            , (OR (-3/8) (3/8) (-1/2) (1/2), posPanel)
            ]
        }

jumpPanel, posPanel :: Double -> State -> IO ()

jumpPanel heightOverWidth state =
    if on_a_floor state
        then do color (Color3 1 0 0 :: Color3 Double)
                renderPrimitive Quads
                  $ sequence_ 
                  $ map vertex
                  [ Vertex2 (-1/2) (-1/2), Vertex2 (-1/2) (1/2)
                  , Vertex2 (1/2) (1/2), Vertex2 (1/2) (-1/2) :: Vertex2 Double
                  ]
        else return ()

posPanel heightOverWidth state = preservingMatrix $
    do -- TODO: use aspect ratio
       color (Color3 1 0 0 :: Color3 Double)
       translate (Vector3 (-1) 0 0 :: Vector3 Double)
       scale (1/1500) (1/125) (1::Double)
       renderString Roman (show (player_pos state))

fill :: IO ()
fill = renderPrimitive Quads $ sequence_ $ map vertex $
  [ Vertex2 (-1) (-1), Vertex2 (-1) 1
  , Vertex2 1 1, Vertex2 1 (-1) :: Vertex2 Double
  ]

enterRect :: OrthoRect -> IO ()
enterRect r = (newMatrix ColumnMajor [(orMaxX r - orMinX r) / 2, 0, 0, 0, 0, (orMaxY r - orMinY r) / 2, 0, 0, 0, 0, 1, 0, (orMaxX r + orMinX r) / 2, (orMaxY r + orMinY r) / 2, 0, 1] :: IO (GLmatrix Double)) >>= multMatrix

aspect :: OrthoRect -> Double
aspect r = (orMaxY r - orMinY r) / (orMaxX r - orMinX r)
