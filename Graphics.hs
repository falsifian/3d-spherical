module Graphics
(   initDisplay
,   display
) where

import Architecture hiding (color)
import Constants
import Data.IORef
import Data.Int
import Engine
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL
import Graphics.UI.GLUT
import GraphicsUtil
import Math
import OSD

initDisplay :: State -> IO State
initDisplay state =
   do -- TODO: Things might go faster if blending is only enabled for the parts that need it.
      blend $= Enabled
      blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
      torusTexture <- makeTorusTexture

      worldDisplayList <- defineNewList Compile display_static_universe
      
      torusDisplayList <- defineNewList Compile (theTorus torusTexture)

      return (state {worldDisplayList = worldDisplayList, torusDisplayList = torusDisplayList, torusTexture = torusTexture})

makeTorusTexture :: IO TextureObject
makeTorusTexture =
    do rowAlignment Unpack $= 1
       [torusTexture] <- genObjectNames 1
       textureBinding Texture2D $= Just torusTexture
       textureWrapMode Texture2D S $= (Repeated, Repeat) -- TODO:  I don't completely understand what each of these two parameters do.
       textureWrapMode Texture2D T $= (Repeated, Repeat) -- TODO:  ditto
       textureFilter Texture2D $= ((Nearest, Nothing), Nearest) -- TODO:  Is this needed?  Is this too slow?  What does the Nothing do?
       pdPtr <- mallocBytes 36
       let imageData = concat [[0, 0, 0, 127-x] | x <- map (127*) [1, 1, 1, 1, 0, 0, 1, 0, 0] :: [Int8]]
       sequence_ [pokeElemOff pdPtr i x | (i, x) <- zip [0 ..] imageData]
       texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D 3 3) 0 (PixelData RGBA Byte pdPtr)
       return torusTexture

display :: IORef State -> IO ()
display state_ref =
    do state <- readIORef state_ref
       display_universe_twice state
       displayOSD defaultOSD state
       swapBuffers

display_universe_twice :: State -> IO ()
display_universe_twice state =
    do depthFunc $= Just Less
       clear [ColorBuffer, DepthBuffer]
       matrixMode $= Projection
       placeCamera (player_pos state) (player_fwd state) (player_up (state_calc state)) (player_right (state_calc state))
       scale4 (-1) (-1) (-1) (-1::Double)
       matrixMode $= Modelview 0
       display_universe_once state
       matrixMode $= Projection
       scale4 (-1) (-1) (-1) (-1::Double)
       matrixMode $= Modelview 0
       clear [DepthBuffer]
       display_universe_once state
       matrixMode $= Projection
       loadIdentity
       matrixMode $= Modelview 0

display_static_universe :: IO ()
display_static_universe =
    do -- bottom sphere (-w pole)
       preservingMatrix $ do scale4 1 1 1 (-1::Double)
			     color (Color3 1 1 1 :: Color3 Double)
			     sphere (bottom_sphere_radius)
       -- x pole
       preservingMatrix $ do swap_wx
			     color (Color3 1 0 0 :: Color3 Double)
			     sphere 0.3
       -- y pole
       preservingMatrix $ do swap_wy
			     color (Color3 0 1 0 :: Color3 Double)
			     sphere 0.3
       -- z pole
       preservingMatrix $ do swap_wz
			     color (Color3 0 0 1 :: Color3 Double)
			     sphere 0.3

       sequence_ $ map draw_ft world_arch

display_universe_once :: State -> IO ()
display_universe_once state =
    do callList (worldDisplayList state)
       if show_torus state then callList (torusDisplayList state) else return ()

theTorus :: TextureObject -> IO ()
theTorus torusTexture =
    let nGridLines = 101
        textureFrequency = 25
        fromParams a b =
            let x0 = cos (a*2*pi) / sqrt 2
                x1 = sin (a*2*pi) / sqrt 2
                x2 = cos (b*2*pi) / sqrt 2
                x3 = sin (b*2*pi) / sqrt 2
            in
            do texCoord (TexCoord2 (a * textureFrequency) (b * textureFrequency) :: TexCoord2 Double)
               color $ calcIllumination universeLights (V4 x0 x1 x2 x3) (V4 x2 x3 (-x0) (-x1)) material
               vertex (Vertex4 x0 x1 x2 x3)
              
        material = Material zvec (V4 0.5 0.5 0.5 1)
    in
    do texture Texture2D $= Enabled
       textureFunction $= Decal
       textureBinding Texture2D $= Just torusTexture
       sequence_ 
         [ renderPrimitive QuadStrip $ sequence_ $ concat
             [ [fromParams a b, fromParams (a + 1/nGridLines) b]
             | b <- [0,1/nGridLines..1]
             ]
         | a <- [0,1/nGridLines..1]
         ]
       texture Texture2D $= Disabled

sphere :: GLdouble -> IO ()
sphere radius = -- radius is the radius in radians
		-- A Euclidean sphere about the origin is a spherical sphere
                -- about the w pole, so we just let GLU do this for us.
                let euclid_radius = tan radius in
                renderQuadric (QuadricStyle Nothing NoTextureCoordinates Outside FillStyle) (Sphere euclid_radius 100 100)
