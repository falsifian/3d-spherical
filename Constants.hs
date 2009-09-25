module Constants where

import qualified Graphics.UI.GLUT as GLUT
import Math

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

advance_rate, up_down_rate, bottom_sphere_radius, gravity, jump_v, player_height, step_tolerance, turn_rate :: Double
advance_rate = 0.8
up_down_rate = 0.8
bottom_sphere_radius = pi / 8
jump_v = 0.6
player_height = pi / 64
step_tolerance = player_height / 2
turn_rate = 5

universeLights :: [(Vec4, Vec3)]
universeLights =
  [ (V4 1 0 0 0, V3 0.5 0 0)
  , (V4 0 1 0 0, V3 0 0.5 0)
  , (V4 0 0 1 0, V3 0 0 0.5)
  , (V4 0 0 0 1, V3 0.5 0.5 0.5)
  ]
