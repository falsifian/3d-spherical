module Engine where

import Constants
import Math

data State = State { player_pos, player_fwd :: Vec4
                   , state_calc :: StateCalc
                   }

data StateCalc = SC { player_up, player_right :: Vec4
                    }

initial_state :: State
initial_state = complete_state $ State (Math.normalize (V4 (tan (bottom_sphere_radius + player_height)) 0 0 (-1))) (V4 0 0 1 0) undefined

complete_state :: State -> State
complete_state state@(State { player_pos = pos, player_fwd = fwd }) =
    let up' = V4 0 0 0 1
        right = Math.normalize (cross4 pos fwd up')
        up = cross4 pos fwd right
    in
    state { state_calc = SC up right }

update :: State -> State
update = id
