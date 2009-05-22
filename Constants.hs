module Constants where

import qualified Graphics.UI.GLUT as GLUT

program_name :: String
program_name = "Spherical Geometry"

window_size :: GLUT.Size
window_size = GLUT.Size 640 640

window_title :: String
window_title = "GLUT Window"

-- Physics

gravity, normal_force_eps, step_size :: Double
gravity = 1
normal_force_eps = 1e-6
step_size = 1e-2

-- The World

bottom_sphere_radius :: Double
bottom_sphere_radius = 0.1

player_height :: Double
player_height = 0.001
