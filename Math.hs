module Math where

import qualified Graphics.Rendering.OpenGL.GL as GL

infixl 7 .*, ./
infixl 6 @+, @-

class Vector v where
    zvec :: v
    (.*) :: v -> Double -> v
    (./) :: v -> Double -> v
    a ./ b = a .* (1 / b)
    (@+) :: v -> v -> v
    (@-) :: v -> v -> v
    a @- b = a @+ b .* (-1)
    vsum :: [v] -> v
    vsum = foldl (@+) zvec

class (NormVector v) => IPVector v where
    (@.) :: v -> v -> Double
    -- norm should be the one derived from the inner product (@.)

class (Vector v) => NormVector v where
    norm :: v -> Double
    norm = sqrt . normSqr
    normSqr :: v -> Double
    normSqr = (^ 2) . norm
    normalize :: v -> v
    normalize x = x ./ norm x

data Vec3 = V3 !Double !Double !Double deriving Show

data Vec4 = V4 !Double !Double !Double !Double deriving Show

instance Vector Vec3 where
    zvec = V3 0 0 0
    (V3 x0 x1 x2) @+ (V3 y0 y1 y2) = V3 (x0 + y0) (x1 + y1) (x2 + y2)
    (V3 x0 x1 x2) .* a = V3 (x0 * a) (x1 * a) (x2 * a)

instance Vector Vec4 where
    zvec = V4 0 0 0 0
    (V4 x0 x1 x2 x3) @+ (V4 y0 y1 y2 y3) = V4 (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
    (V4 x0 x1 x2 x3) .* a = V4 (x0 * a) (x1 * a) (x2 * a) (x3 * a)

instance IPVector Vec3 where
    (V3 x0 x1 x2) @. (V3 y0 y1 y2) = (x0 * y0) + (x1 * y1) + (x2 * y2)

instance IPVector Vec4 where
    (V4 x0 x1 x2 x3) @. (V4 y0 y1 y2 y3) = (x0 * y0) + (x1 * y1) + (x2 * y2) + (x3 * y3)

instance NormVector Vec3 where
    normSqr (V3 x y z) = x ^ 2 + y ^ 2 + z ^ 2

instance NormVector Vec4 where
    normSqr (V4 x y z w) = x ^ 2 + y ^ 2 + z ^ 2 + w ^ 2

zero_w :: Vec3 -> Vec4
zero_w (V3 x y z) = V4 x y z 0

cross :: Vec3 -> Vec3 -> Vec3
cross (V3 x0 x1 x2) (V3 y0 y1 y2) =
    V3 (x1 * y2 - x2 * y1) (- x0 * y2 + x2 * y0) (x0 * y1 - x1 * y0)

cross4 :: Vec4 -> Vec4 -> Vec4 -> Vec4
cross4 (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3) (V4 z0 z1 z2 z3) =
    V4
        (det3 x1 x2 x3 y1 y2 y3 z1 z2 z3)
        (- det3 x0 x2 x3 y0 y2 y3 z0 z2 z3)
        (det3 x0 x1 x3 y0 y1 y3 z0 z1 z3)
        (- det3 x0 x1 x2 y0 y1 y2 z0 z1 z2)
    where
        det3 x0 x1 x2 y0 y1 y2 z0 z1 z2 =
            x0 * y1 * z2
          - x0 * y2 * z1
          - x1 * y0 * z2
          + x1 * y2 * z0
          + x2 * y0 * z1
          - x2 * y1 * z0

make_ortho :: (IPVector v) => v -> v -> v
make_ortho a b = b @- a .* (a @. b)

sph_add :: Vec4 -> Vec4 -> Vec4
sph_add p v = if norm v < 1e-9 then p else normalize (p @+ v .* (sin (norm v) / norm v))

sph_dist :: Vec4 -> Vec4 -> Double
sph_dist x y = acos (1 - normSqr (x @- y) / 2)

vec4_to_vertex4 :: Vec4 -> GL.Vertex4 Double
vec4_to_vertex4 (V4 x y z w) = GL.Vertex4 x y z w

lose_w :: Vec4 -> Vec3
lose_w (V4 x y z _) = V3 x y z

sph_within_tri :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Bool
-- The triangle must be given counter-clockwise.
sph_within_tri a b c x = cross a (b @- a) @. (x @- a) > 0 && cross b (c @- b) @. (x @- b) > 0 && cross c (a @- c) @. (x @- c) > 0
