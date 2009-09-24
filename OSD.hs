module OSD
(   OSD
,   defaultOSD
,   displayOSD
) where

import Engine
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import Math

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
            , (OR (1/32) (7/32) (-3/8) (3/8), coordMeterPanel (V4 1 0 0 0 @.))
            , (OR (9/32) (15/32) (-3/8) (3/8), coordMeterPanel (V4 0 1 0 0 @.))
            , (OR (17/32) (23/32) (-3/8) (3/8), coordMeterPanel (V4 0 0 1 0 @.))
            , (OR (25/32) (31/32) (-3/8) (3/8), coordMeterPanel (V4 0 0 0 1 @.))
            ]
        }

jumpPanel :: Double -> State -> IO ()
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

coordMeterPanel :: (Vec4 -> Double) -> Double -> State -> IO ()
coordMeterPanel coord heightOverWidth state =
    let minAngle = -pi/4
        maxAngle = 5*pi/4
        nTics = 9
        ticInnerRad = 3/4
        ticOuterRad = 15/16
        arrowBaseLength = ticOuterRad - ticInnerRad
        arrowLength = (ticInnerRad + ticOuterRad) / 2
    in
    do color (Color3 1 1 1 :: Color3 Double)
       renderPrimitive Lines $ sequence_ $ concat
           [ let angle = (minAngle * fromIntegral (nTics - i - 1) + maxAngle * fromIntegral i) / fromIntegral (nTics - 1) in
             [ vertex (Vertex2 (ticInnerRad * cos angle) (ticInnerRad * sin angle) :: Vertex2 Double)
             , vertex (Vertex2 (ticOuterRad * cos angle) (ticOuterRad * sin angle) :: Vertex2 Double)
             ]
           | i <- [0..pred nTics]
           ]
       unsafePreservingMatrix $
           do rotate ((minAngle + (coord (player_pos state) + 1) * (maxAngle - minAngle) / 2) * 180 / pi) (Vector3 0 0 1)
              color (Color3 1 0 0 :: Color3 Double)
              renderPrimitive Triangles $ 
                  do vertex (Vertex2 0 (-arrowBaseLength / 2) :: Vertex2 Double)
                     vertex (Vertex2 0 (arrowBaseLength / 2) :: Vertex2 Double)
                     vertex (Vertex2 arrowLength 0 :: Vertex2 Double)

fill :: IO ()
fill = renderPrimitive Quads $ sequence_ $ map vertex $
  [ Vertex2 (-1) (-1), Vertex2 (-1) 1
  , Vertex2 1 1, Vertex2 1 (-1) :: Vertex2 Double
  ]

enterRect :: OrthoRect -> IO ()
enterRect r = (newMatrix ColumnMajor [(orMaxX r - orMinX r) / 2, 0, 0, 0, 0, (orMaxY r - orMinY r) / 2, 0, 0, 0, 0, 1, 0, (orMaxX r + orMinX r) / 2, (orMaxY r + orMinY r) / 2, 0, 1] :: IO (GLmatrix Double)) >>= multMatrix

aspect :: OrthoRect -> Double
aspect r = (orMaxY r - orMinY r) / (orMaxX r - orMinX r)
