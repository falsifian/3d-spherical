module Engine
( State(..)
, StateCalc(..)
, initial_state
, update
) where

import Constants
import Math

data State = State { player_pos, player_fwd, player_v :: Vec4
                   , state_calc :: StateCalc
                   }

data StateCalc = SC { player_up, player_right :: Vec4
                    }

initial_state :: State
initial_state = complete_state $ State (Math.normalize (V4 (tan (2 * bottom_sphere_radius + player_height)) 0 0 (-1))) (V4 0 0 1 0) zvec undefined

complete_state :: State -> State
complete_state state@(State { player_pos = pos, player_fwd = fwd }) =
    let up' = V4 0 0 0 1
        right = Math.normalize (cross4 pos fwd up')
        up = cross4 pos right fwd
    in
    state { state_calc = SC up right }

update :: State -> State
update = complete_state . move_forward

move_forward :: State -> State
move_forward state@(State { player_pos = pos, player_fwd = fwd, player_v = v, state_calc = SC { player_up = up, player_right = right } }) =
    let pos' = ensure_above_ground $ normalize (pos @+ v .* step_size)
        fwd' = normalize (cross4 pos' (V4 0 0 0 1) right)
        v' = make_ortho pos' $ if really_above_ground pos' then v @+ V4 0 0 0 (- 1) .* (gravity * step_size) else zvec
    in
    state { player_pos = pos', player_fwd = fwd', player_v = v' }

mostly_above_ground, really_above_ground :: Vec4 -> Bool
mostly_above_ground (V4 _ _ _ w) = - w < cos bottom_sphere_radius + normal_force_eps
really_above_ground (V4 _ _ _ w) = - w < cos bottom_sphere_radius - normal_force_eps

ensure_above_ground :: Vec4 -> Vec4
ensure_above_ground p =
    if mostly_above_ground p then p else
        let V4 x y z w = p
            n = sqrt (x^2+y^2+z^2) 
            m = tan bottom_sphere_radius / n
        in
        normalize (V4 (x * m) (y * m) (z * m) (-1))
