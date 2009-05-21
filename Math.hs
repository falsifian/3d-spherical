module Math where

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

class (Vector v) => NormVector v where
	norm :: v -> Double
	norm = sqrt . normSqr
	normSqr :: v -> Double
	normSqr = (^ 2) . norm
	normalize :: v -> v
	normalize x = x ./ norm x

data Vec4 = V4 !Double !Double !Double !Double deriving Show

instance Vector Vec4 where
	zvec = V4 0 0 0 0
	(V4 x0 x1 x2 x3) @+ (V4 y0 y1 y2 y3) = V4 (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
	(V4 x0 x1 x2 x3) .* a = V4 (x0 * a) (x1 * a) (x2 * a) (x3 * a)

instance NormVector Vec4 where
	normSqr (V4 x0 x1 x2 x3) = x0 ^ 2 + x1 ^ 2 + x2 ^ 2 + x3 ^ 2

cross4 :: Vec4 -> Vec4 -> Vec4 -> Vec4
cross4 (V4 x0 x1 x2 x3) (V4 y0 y1 y2 y3) (V4 z0 z1 z2 z3) =
    V4
        (det3 x1 x2 x3 y1 y2 y3 z1 z2 z3)
        (det3 x2 x3 x0 y2 y3 y0 z2 z3 z0)
        (det3 x3 x0 x1 y3 y0 y1 z3 z0 z1)
        (det3 x0 x1 x2 y0 y1 y2 z0 z1 z2)
    where
        det3 x0 x1 x2 y0 y1 y2 z0 z1 z2 =
            x0 * y1 * z2
          - x0 * y2 * z1
          - x1 * y0 * z2
          + x1 * y2 * z0
          + x2 * y0 * z1
          - x2 * y1 * z0
