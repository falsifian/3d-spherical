module Engine
( Key(..)
, State(..)
, StateCalc(..)
, initial_state
, update
) where

import Architecture
import Constants
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Graphics.Rendering.OpenGL.GL as GL
import Math

data Key = KFwd | KBwd | KRight | KLeft | KJump | KHover | KUp | KDown | KTorus deriving (Eq, Ord)

data State = State { player_pos, player_fwd :: Vec4
                   , player_vert_v :: Double
                   , on_a_floor, hover, hover_released, show_torus, show_torus_released :: Bool
                   , keys :: Set.Set Key
                   , worldDisplayList, torusDisplayList :: GL.DisplayList
                   , state_calc :: StateCalc
                   , torusTexture :: GL.TextureObject
                   }

data StateCalc = SC { player_up, player_right :: Vec4
                    }

initial_state :: State
initial_state = complete_state $ State (Math.normalize (V4 (tan (2 * bottom_sphere_radius + player_height)) 0 0 (-1))) (V4 0 0 1 0) 0 False False False False False (Set.empty) undefined undefined undefined undefined

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
        pos_after_vert =
            if hover state
                then case (Set.member KDown (keys state), Set.member KUp (keys state)) of
                    (True, False) -> sph_add pos_after_walk (up .* (- up_down_rate * step_size))
                    (False, True) -> sph_add pos_after_walk (up .* (up_down_rate * step_size))
                    _ -> pos_after_walk
                else sph_add pos_after_walk (up .* (player_vert_v state * step_size))
        height_after_vert = sph_dist pos_after_vert (V4 0 0 0 (- 1))
        on_ground =
            if height_after_vert < bottom_sphere_radius + player_height + normal_force_eps 
                then Just (bottom_sphere_radius + player_height)
                else Nothing
        on_triangles =
            [
                if sph_within_tri a b c (normalize $ lose_w pos_after_vert) && height_after_vert > height + player_height - step_tolerance - normal_force_eps && height_after_vert < height + player_height + normal_force_eps 
                    then Just (height + player_height)
                    else Nothing
            | FTri { height = height, a = a, b = b, c = c } <- world_arch
            ]
        floor_height = guard (not (hover state) && player_vert_v state <= 0) >>
            -- Actually, this is the floor height plus the player's height.
            let l = catMaybes (on_ground : on_triangles) in
            if null l then Nothing else Just (maximum l)
        new_pos =
            case floor_height of
                Just h -> let V4 x y z w = pos_after_vert
                          in
                          sph_add (V4 0 0 0 (- 1)) (normalize (V4 x y z 0) .* h)
                Nothing -> pos_after_vert
        fwd_after_turn = case (Set.member KLeft (keys state), Set.member KRight (keys state)) of
            (True, False) -> fwd .* cos (turn_rate * step_size) @- right .* sin (turn_rate * step_size)
            (False, True) -> fwd .* cos (turn_rate * step_size) @+ right .* sin (turn_rate * step_size)
            _ -> fwd
        new_fwd = normalize $ cross4 new_pos (V4 0 0 0 1) (cross4 new_pos fwd_after_turn (V4 0 0 0 1))
        new_vert_v =
            if isJust floor_height
                then if Set.member KJump (keys state) then jump_v else 0
                else player_vert_v state - gravity * step_size
        new_hover = hover state /= (Set.member KHover (keys state) && hover_released state)
        new_hover_released = not (Set.member KHover (keys state))
        new_show_torus = show_torus state /= (Set.member KTorus (keys state) && show_torus_released state)
        new_show_torus_released = not (Set.member KTorus (keys state))
        ret = complete_state (state { player_pos = new_pos, player_fwd = new_fwd, player_vert_v = new_vert_v, on_a_floor = isJust floor_height, hover = new_hover, hover_released = new_hover_released, show_torus = new_show_torus, show_torus_released = new_show_torus_released })
    in seq (verify_state ret) ret -- TODO: get rid of expensive state verification
