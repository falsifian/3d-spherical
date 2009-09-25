module Architecture where

import Constants
import qualified Graphics.Rendering.OpenGL.GL as GL
import GraphicsUtil
import List
import Math

data Floating_Triangle = FTri { height :: Double
                              , color :: Vec3
                              , side_segments :: Int
                              , a, b, c :: Vec3 -- counter-clockwise
                              }

world_arch :: [Floating_Triangle]
world_arch = concat [afloor i | i <- [1 .. 7]]

afloor :: Int -> [Floating_Triangle]
afloor i = [FTri h c 20 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1), FTri (h - pi / 16) (V3 1 0.5 0.5) 10 (normalize (V3 0.5 1 1)) (normalize (V3 1 0.5 1)) (normalize (V3 1 1 0.5))]
    where
        h = fromIntegral i * pi / 8
        c = V3 (fromIntegral i / 8) (fromIntegral (mod i 2)) 0.5

draw_ft :: Floating_Triangle -> IO ()
draw_ft (FTri height color side_segments a b c) =
    let
        one_row i = map (fix_height . interp) $
            (i, side_segments - i, 0) :
            concat [[(i + 1, side_segments - i - j, j - 1), (i, side_segments - i - j, j)] | j <- [1 .. side_segments - i]]
        fix_height x = sph_add (V4 0 0 0 (- 1)) (normalize (zero_w x) .* height)
        interp (i, j, k) = a .* (fromIntegral i / fromIntegral side_segments) @+
                           b .* (fromIntegral j / fromIntegral side_segments) @+
                           c .* (fromIntegral k / fromIntegral side_segments)
        makeVertexSpec v@(V4 _ _ _ w) = (v, normalize (V4 0 0 0 1 @- v .* w), Material zvec (extendWithOne color))
        extendWithOne (V3 x y z) = V4 x y z 1
    in 
    do sequence_ [GL.renderPrimitive GL.TriangleStrip $ drawWithIllumination universeLights $ map makeVertexSpec (one_row i) | i <- [0 .. pred side_segments]]
