module GraphicsUtil
(   Material(..)
,   diffuseReflection
,   drawWithIllumination
,   scale4
,   swap_wx
,   swap_wy
,   swap_wz
,   placeCamera
) where

import Graphics.Rendering.OpenGL.GL
import Math

data Material = Material {emitColour, diffuseColour :: Vec4}

diffuseReflection :: Vec4 -> Vec4 -> Vec4 -> Double
diffuseReflection lightPos objectPos objectNormal =
    let lightDir = Math.normalize (flatten (lightPos @- objectPos))
        flatten v = v @- (objectPos .* (objectPos @. v))
    in abs (objectNormal @. lightDir)

-- drawWithIllumination is a convenience function which decides the
-- illumination of every vertex using diffuseReflection.  This might be
-- extended, for example to use shaders for new kinds of lights.
--
-- Note: OpenGL's transformation matrices are ignored here.
drawWithIllumination :: [(Vec4, Vec3)] -> [(Vec4, Vec4, Material)] -> IO ()
drawWithIllumination lightSources vertices =
    let extendWithOne (V3 x y z) = V4 x y z 1
    in
    sequence_
      [ let diffuseSum = vsum [ lightColour .* diffuseReflection lightPos objectPos objectNormal
                              | (lightPos, lightColour) <- lightSources
                              ]
        in
        do color $ vec4_to_color4 $ emitColour material @+ extendWithOne diffuseSum @* diffuseColour material
           vertex $ vec4_to_vertex4 objectPos
      | (objectPos, objectNormal, material) <- vertices
      ]

newGLMatrix :: (MatrixComponent c) => MatrixOrder -> [c] -> IO (GLmatrix c)
newGLMatrix = newMatrix

scale4 :: (MatrixComponent c, Num c) => c -> c -> c -> c -> IO ()
scale4 x y z w = newGLMatrix ColumnMajor [x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, w] >>= multMatrix

swap_wx :: IO ()
swap_wx = (newMatrix ColumnMajor [0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0] :: IO (GLmatrix Double)) >>= multMatrix

swap_wy :: IO ()
swap_wy = (newMatrix ColumnMajor [1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0] :: IO (GLmatrix Double)) >>= multMatrix

swap_wz :: IO ()
swap_wz = (newMatrix ColumnMajor [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0] :: IO (GLmatrix Double)) >>= multMatrix

placeCamera :: Vec4 -> Vec4 -> Vec4 -> Vec4 -> IO ()
placeCamera pos fwd up right =
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
