module Constants where

import qualified Graphics.UI.GLUT as GLUT

program_name :: String
program_name = "Spherical Geometry"

window_size :: GLUT.Size
window_size = GLUT.Size 640 640

window_title :: String
window_title = "GLUT Window"

-- The world.

bottom_sphere_radius :: Double
bottom_sphere_radius = 0.1

player_height :: Double
player_height = 0.001
