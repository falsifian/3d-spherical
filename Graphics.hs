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
import GraphicsUtil
import OSD

initDisplay :: IO ()
initDisplay =
    do -- TODO: Things might go faster if blending is only enabled for the parts that need it.
       blend $= Enabled
       blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

display :: IORef State -> IO ()
display state_ref =
    do state <- readIORef state_ref
       display_universe_twice state
       displayOSD defaultOSD state
       swapBuffers

display_universe_twice :: State -> IO ()
display_universe_twice state =
    do depthFunc $= Just Less
       clear [ColorBuffer, DepthBuffer]
       matrixMode $= Projection
       placeCamera (player_pos state) (player_fwd state) (player_up (state_calc state)) (player_right (state_calc state))
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

       -- The Torus
       color (Color3 0.5 0.5 0.5 :: Color3 Double)
       theTorus

       sequence_ $ map draw_ft world_arch

theTorus :: IO ()
theTorus =
    let nGridLines = 101
        fromParams a b = Vertex4 (cos (a*2*pi)) (sin (a*2*pi)) (cos (b*2*pi)) (sin (b*2*pi)) :: Vertex4 Double
    in
    sequence_ 
    [ renderPrimitive QuadStrip $ sequence_ $ map vertex $ concat
        [ [fromParams a b, fromParams (a + 1/nGridLines) b]
        | b <- [0,1/nGridLines..1]
        ]
    | a <- [0,1/nGridLines..1]
    ]

sphere :: GLdouble -> IO ()
sphere radius = -- radius is the radius in radians
		-- A Euclidean sphere about the origin is a spherical sphere
                -- about the w pole, so we just let GLU do this for us.
                let euclid_radius = tan radius in
                renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere euclid_radius 100 100)
