module Architecture where

import Constants
import qualified Graphics.Rendering.OpenGL.GL as GL
import List
import Math

data Floating_Triangle = FTri { height :: Double
                              , color :: GL.Color3 Double
                              , side_segments :: Int
                              , a, b, c :: Vec3 -- counter-clockwise
                              }

world_arch :: [Floating_Triangle]
world_arch = [FTri 0.115 (GL.Color3 1 0 1) 20 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1), FTri 0.117 (GL.Color3 0 1 1) 20 (V3 0 1 0) (V3 1 0 0) (normalize (V3 0.5 0.5 (-0.5)))] ++ stairs

stairs :: [Floating_Triangle]
stairs = concat
    [   let p0 = normalize (V3 (cos (t0 * r)) (sin (t0 * r)) (-zw / 2))
            p1 = normalize (V3 (cos (t1 * r)) (sin (t1 * r)) (-zw / 2))
            p2 = normalize (V3 (cos (t1 * r)) (sin (t1 * r)) (zw / 2))
            p3 = normalize (V3 (cos (t0 * r)) (sin (t0 * r)) (zw / 2))
        in
        [FTri (t0 * pi) (GL.Color3 0.7 0.5 0.2) 10 p0 p1 p2, FTri (t0 * pi) (GL.Color3 0.7 0.5 0.2) 10 p0 p2 p3]
    | t0 <- [0.1 / pi, 0.11 / pi .. 0.2 / pi], t1 <- [t0 + 0.01 / pi]
    ]
    where r = 100
          zw = 0.2

draw_ft :: Floating_Triangle -> IO ()
draw_ft (FTri height color side_segments a b c) =
    do GL.color color
       sequence_ [GL.renderPrimitive GL.TriangleStrip $ sequence_ $ map GL.vertex (one_row i) | i <- [0 .. pred side_segments]]
    where
        one_row i = map (vec4_to_vertex4 . fix_height . interp) $
            (i, side_segments - i, 0) :
            concat [[(i + 1, side_segments - i - j, j - 1), (i, side_segments - i - j, j)] | j <- [1 .. side_segments - i]]
        fix_height x = sph_add (V4 0 0 0 (- 1)) (normalize (zero_w x) .* height)
        interp (i, j, k) = a .* (fromIntegral i / fromIntegral side_segments) @+
                           b .* (fromIntegral j / fromIntegral side_segments) @+
                           c .* (fromIntegral k / fromIntegral side_segments)
