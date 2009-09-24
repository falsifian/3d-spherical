module OSD
(   OSD
,   defaultOSD
,   displayOSD
) where

import Engine
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT

data OSD =
    OSD { osdRect :: OrthoRect
        , osdBackgroundColour, osdPanelColour :: Color4 Double
        , osdPanels :: [(OrthoRect, Double -> State -> IO ())]
        }

data OrthoRect = OR { orMinX, orMaxX, orMinY, orMaxY :: Double }

displayOSD :: OSD -> State -> IO ()
displayOSD osd state =
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
