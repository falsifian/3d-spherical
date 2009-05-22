module Engine
( Key(..)
, State(..)
, StateCalc(..)
, initial_state
, update
) where

import Constants
import List
import Math
import qualified Data.Set as Set

data Key = KFwd | KBwd | KRight | KLeft | KJump deriving (Eq, Ord)

data State = State { player_pos, player_fwd, player_v :: Vec4
                   , keys :: Set.Set Key
                   , mostly_ab, really_ab :: Bool
                   , state_calc :: StateCalc
                   }

data StateCalc = SC { player_up, player_right :: Vec4
                    }

initial_state :: State
initial_state = complete_state $ State (Math.normalize (V4 (tan (2 * bottom_sphere_radius + player_height)) 0 0 (-1))) (V4 0 0 1 0) zvec (Set.empty) False False undefined

orthonormal :: (IPVector v) => [v] -> Bool
orthonormal vs = and [abs (normSqr x - 1) < 1e-9 | x <- vs] && and [case xs of [] -> True; h : t -> and [abs (h @. y) < 1e-9 | y <- t] | xs <- tails vs]

verify_state :: State -> ()
verify_state state =
    if not (orthonormal [player_pos state, player_fwd state, player_up (state_calc state), player_right (state_calc state)]) then error "verify_state: player vectors not orthonormal"
    else if or [abs (V4 0 0 0 1 @. x) > 1e-9 | x <- [player_fwd state, player_right (state_calc state)]] then error "verify_state:  A player vector other than pos or up is not orthogonal to the w axis."
    else ()

complete_state :: State -> State
complete_state state@(State { player_pos = pos, player_fwd = fwd }) =
    let up' = V4 0 0 0 1
        right = Math.normalize (cross4 pos fwd up')
        up = cross4 pos right fwd
    in
    state { state_calc = SC up right }

update :: State -> State
update state@(State { player_pos = pos, player_fwd = fwd, player_v = v, state_calc = SC { player_up = up, player_right = right } }) =
    let new_pos = ensure_above_ground $ normalize (pos @+ v_for_updating_pos .* step_size)
        fwd_after_turn = case (Set.member KLeft (keys state), Set.member KRight (keys state)) of
            (True, False) -> fwd @- right .* (turn_rate * step_size)
            (False, True) -> fwd @+ right .* (turn_rate * step_size)
            _ -> fwd
        new_fwd = normalize $ cross4 new_pos (V4 0 0 0 1) (cross4 new_pos fwd_after_turn (V4 0 0 0 1))
        v_after_gravity = if really_above_ground pos then v @+ V4 0 0 0 (- 1) .* (gravity * step_size) else zvec
        v_for_updating_pos = case (Set.member KFwd (keys state), Set.member KBwd (keys state)) of
            (True, False) -> v_after_gravity @+ fwd .* (advance_rate * step_size)
            (False, True) -> v_after_gravity @- fwd .* (advance_rate * step_size)
            _ -> v_after_gravity
        new_v = make_ortho new_pos v_for_updating_pos
        ret = complete_state (state { player_pos = new_pos, player_fwd = new_fwd, player_v = new_v, mostly_ab = mostly_above_ground pos, really_ab = really_above_ground pos })
    in seq (verify_state ret) ret -- TODO: get rid of expensive state verification

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
