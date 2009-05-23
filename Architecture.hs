module Architecture where

import qualified Graphics.Rendering.OpenGL.GL as GL
import List
import Math

data Floating_Triangle = FTri { height :: Double
                              , color :: GL.Color3 Double
                              , a, b, c :: Vec3 -- counter-clockwise
                              }

world_arch :: [Floating_Triangle]
world_arch = [FTri 0.115 (GL.Color3 1 0 1) (V3 1 0 0) (V3 0 1 0) (V3 0 0 1), FTri 0.117 (GL.Color3 0 1 1) (V3 0 1 0) (V3 1 0 0) (normalize (V3 0.5 0.5 (-0.5)))]

draw_ft :: Floating_Triangle -> IO ()
draw_ft (FTri height color a b c) =
    do GL.color color
       GL.renderPrimitive GL.TriangleStrip $ sequence_ $
           concat [GL.primitiveRestart : map GL.vertex (one_row i) | i <- [0 .. pred side_segments]]
    where
        side_segments = 10 -- TODO: calculate this properly
        one_row i = map (vec4_to_vertex4 . fix_height . interp) $
            (i, side_segments - i, 0) :
            concat [[(i + 1, side_segments - i - j, j - 1), (i, side_segments - i - j, j)] | j <- [1 .. side_segments - i]]
        fix_height x = sph_add (V4 0 0 0 (- 1)) (normalize (zero_w x) .* height)
        interp (i, j, k) = a .* (fromIntegral i / fromIntegral side_segments) @+
                           b .* (fromIntegral j / fromIntegral side_segments) @+
                           c .* (fromIntegral k / fromIntegral side_segments)
