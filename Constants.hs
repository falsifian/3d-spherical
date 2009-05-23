module Constants where

import qualified Graphics.UI.GLUT as GLUT

program_name :: String
program_name = "Spherical Geometry"

window_size :: GLUT.Size
window_size = GLUT.Size 640 640

window_title :: String
window_title = "GLUT Window"

-- Physics

advance_rate, gravity, jump_v, normal_force_eps, step_size, step_tolerance, turn_rate :: Double
advance_rate = 0.4
gravity = 1
jump_v = 0.2
normal_force_eps = 1e-6
step_size = 1e-2
step_tolerance = player_height / 2
turn_rate = 5

-- The World

bottom_sphere_radius :: Double
bottom_sphere_radius = 0.1

player_height :: Double
player_height = 0.01
