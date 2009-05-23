module Constants where

import qualified Graphics.UI.GLUT as GLUT

program_name :: String
program_name = "Spherical Geometry"

window_size :: GLUT.Size
window_size = GLUT.Size 640 640

window_title :: String
window_title = "GLUT Window"

-- Physics

normal_force_eps, step_size :: Double
gravity = 1
normal_force_eps = 1e-6
step_size = 1e-2

-- The World


advance_rate, bottom_sphere_radius, gravity, jump_v, player_height, step_tolerance, turn_rate :: Double
advance_rate = 0.4
bottom_sphere_radius = pi / 8
jump_v = 0.6
player_height = pi / 16
step_tolerance = player_height / 2
turn_rate = 5
