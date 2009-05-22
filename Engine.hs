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

data State = State { player_pos, player_fwd :: Vec4
                   , player_vert_v :: Double
                   , on_the_ground :: Bool
                   , keys :: Set.Set Key
                   , state_calc :: StateCalc
                   }

data StateCalc = SC { player_up, player_right :: Vec4
                    }

initial_state :: State
initial_state = complete_state $ State (Math.normalize (V4 (tan (2 * bottom_sphere_radius + player_height)) 0 0 (-1))) (V4 0 0 1 0) 0 False (Set.empty) undefined

orthonormal :: (IPVector v) => [v] -> Bool
orthonormal vs = and [abs (normSqr x - 1) < 1e-9 | x <- vs] && and [case xs of [] -> True; h : t -> and [abs (h @. y) < 1e-9 | y <- t] | xs <- tails vs]

verify_state :: State -> ()
verify_state state =
    let player_vectors = [player_pos state, player_fwd state, player_up (state_calc state), player_right (state_calc state)] in
    if not (orthonormal player_vectors) then error ("verify_state: player vectors not orthonormal: " ++ show player_vectors)
    else if or [abs (V4 0 0 0 1 @. x) > 1e-9 | x <- [player_fwd state, player_right (state_calc state)]] then error "verify_state:  A player vector other than pos or up is not orthogonal to the w axis."
    else ()

complete_state :: State -> State
complete_state state@(State { player_pos = pos, player_fwd = fwd }) =
    let up' = V4 0 0 0 1
        right = Math.normalize (cross4 pos fwd up')
        up = cross4 pos right fwd
    in
    if abs (normSqr right - 1) > 1e-10 then error "complete_state: boom!" else
    state { state_calc = SC up right }

update :: State -> State
update state@(State { player_pos = pos, player_fwd = fwd, state_calc = SC { player_up = up, player_right = right } }) =
    let pos_after_walk = case (Set.member KFwd (keys state), Set.member KBwd (keys state)) of
            (True, False) -> sph_add pos (fwd .* (advance_rate * step_size))
            (False, True) -> sph_add pos (fwd .* (- advance_rate * step_size))
            _ -> pos
        pos_after_vert = sph_add pos_after_walk (up .* (player_vert_v state * step_size))
        on_the_ground = sph_dist pos_after_vert (V4 0 0 0 (- 1)) < bottom_sphere_radius + player_height + normal_force_eps
        new_pos =
            if on_the_ground
                then let V4 x y z w = pos_after_vert
                     in
                     sph_add (V4 0 0 0 (- 1)) (normalize (V4 x y z 0) .* (bottom_sphere_radius + player_height))
                else pos_after_vert
        fwd_after_turn = case (Set.member KLeft (keys state), Set.member KRight (keys state)) of
            (True, False) -> fwd .* cos (turn_rate * step_size) @- right .* sin (turn_rate * step_size)
            (False, True) -> fwd .* cos (turn_rate * step_size) @+ right .* sin (turn_rate * step_size)
            _ -> fwd
        new_fwd = normalize $ cross4 new_pos (V4 0 0 0 1) (cross4 new_pos fwd_after_turn (V4 0 0 0 1))
        new_vert_v =
            if on_the_ground
                then if Set.member KJump (keys state) then jump_v else 0
                else player_vert_v state - gravity * step_size
        ret = complete_state (state { player_pos = new_pos, player_fwd = new_fwd, player_vert_v = new_vert_v, on_the_ground = on_the_ground })
    in seq (verify_state ret) ret -- TODO: get rid of expensive state verification
