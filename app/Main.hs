{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Linear
import Control.Lens
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Color
import Text.Show.Functions

import Types
import Bodies
import Rendering

{-
------------------------------------ 
- The Comprehensive Control Manual -
------------------------------------
- Esc: Quit
-
--View--
- E: Zoom out
- Q: Zoom in
- R: Reset Zoom to 1
- Number n from 1-8: Warp time by a factor of 2^n
-
--Steering--
- W/S: Point Rocket Pro-/Retrograde
- A/D: Point Rocket Radially In/Out
- Left/Right: Turn Rocket
- Up: ACTIVATE ENGINE
------------------------------------}

--Parameters
thrust = 100 
agility = 1+pi
zoomSpeed = 7 

--Keypress detection
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k -> parseEvents (delete k keysDown)
        _ -> parseEvents keysDown

keyDown :: Set SDL.Keysym -> SDL.SDLKey -> Bool
keyDown s k = not . null . filter ((== k) . SDL.symKey) $ s

deriving instance Ord SDL.Keysym

--Key processing
getWarp :: Set SDL.Keysym -> Double
getWarp ks = 
    let keyDown' = keyDown ks
    in if keyDown' (SDL.SDLK_1) then 2
  else if keyDown' (SDL.SDLK_2) then 4
  else if keyDown' (SDL.SDLK_3) then 8
  else if keyDown' (SDL.SDLK_4) then 16
  else if keyDown' (SDL.SDLK_5) then 32
  else if keyDown' (SDL.SDLK_6) then 64
  else if keyDown' (SDL.SDLK_7) then 128
  else if keyDown' (SDL.SDLK_8) then 256
  else 1 

getZoom :: Set SDL.Keysym -> Double
getZoom ks = 
    let  keyDown' = keyDown ks
    in if keyDown' (SDL.SDLK_q) then zoomSpeed
  else if keyDown' (SDL.SDLK_e) then (-zoomSpeed)
  else 0

getOrientation :: Set SDL.Keysym -> Double -> (GameState -> Double)
getOrientation ks dt =
    let  keyDown' = keyDown ks
         offset  = if keyDown' (SDL.SDLK_w) then Just 0  
              else if keyDown' (SDL.SDLK_s) then Just pi
              else if keyDown' (SDL.SDLK_a) then Just (pi/2)
              else if keyDown' (SDL.SDLK_d) then Just (-pi/2)
              else Nothing 
         turning_rate = if keyDown' (SDL.SDLK_LEFT)  then agility
                   else if keyDown' (SDL.SDLK_RIGHT) then (-agility)
                   else 0
    in case offset of
            Just x  -> (+x) . getA . vel . rocket
            Nothing -> (+(dt*turning_rate)) . orientation . rocket
        
--Core Logic
start :: GameState
start = Running
    { rocket = Rocket
        { acc = V2 0 0
        , vel = V2 0 (sqrt$(mass theSun)/((pos . rocket $ start)^._x))
        , pos = V2 ((*1.1).size$theSun) 0 
        , orientation = 0 }
    , camPos = pos . rocket $ start
    , camZoom = 1 
    , worldTime = 0 
    , solarSystem = theSolarSystem }

runGame :: (Monad m, HasTime t s) => GameState -> Wire s () m (Set SDL.Keysym) GameState
runGame prevF = mkPure $ \ds keys -> (Right prevF, runGame (nextFrame ds keys prevF))   

gameW :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) GameState
gameW = runGame start

nextFrame :: (HasTime a t) => t -> Set SDL.Keysym -> GameState -> GameState
nextFrame ds ks prevF  = 
    let keyDown' = keyDown $ ks
        warpF = getWarp ks
        dt = (*warpF) . realToFrac . dtime $ ds
        
        --View Zoom
        zoomRate = getZoom ks
        zoom' = if keyDown' (SDL.SDLK_r) then 1
                else (*(1+dt*zoomRate)) . camZoom $ prevF

        --Update Solar System
        solarSystem' = updateSystem (worldTime prevF) (solarSystem prevF)

        --Ship Steering
        or' = getOrientation ks dt $ prevF
        -- In Thrust We Trust --
        engine_acc = (^*engine_Power) . angle $ or'
        engine_Power = if keyDown' (SDL.SDLK_UP) then thrust 
                       else 0
        
        --Gravity
        gravt_acc = sum . map (gravity$pos . rocket$prevF) $ solarSystem'
         
        --'Integrate' the next velocity/position
        acc' = gravt_acc + engine_acc
        vel' = (vel$rocket prevF) + (dt *^ acc')
        pos' = (pos$rocket prevF) + (dt *^ vel')
        
       in if keyDown' (SDL.SDLK_ESCAPE) then Over 
        else Running{ rocket = Rocket
                        { acc = acc'
                        , vel = vel'
                        , pos = pos'
                        , orientation = or' }
                    , camPos = pos' 
                    , camZoom = zoom'
                    , worldTime = dt+(worldTime prevF)
                    , solarSystem = solarSystem' }


gravity :: V2 Double -> CelestialBody -> V2 Double
gravity player body = vec ^* ((mass body)/((**1.5).quadrance $ vec)) 
    where vec = (bodyPos body) - player

updateSystem :: Double -> [CelestialBody] -> [CelestialBody]
updateSystem time sys = map (\cb -> cb {bodyPos = orbit cb $ time }) sys

--------------------------------------------------------------

width' = fromIntegral width
height' = fromIntegral height

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setCaption "Ikarus" ""
    screen <- SDL.setVideoMode width' height' 32 [SDL.SWSurface]
    void $ go empty screen clockSession_ gameW

    where 
    go keysDown screen cses gW = do
        keysDown' <- parseEvents keysDown
        (ds, cses') <- stepSession cses 
        (eg, gW') <- stepWire gW ds (Right keysDown')
        let ng = either (const start) id eg
        putStrLn $ show ds
        x <- renderFrame screen ng  
        if x then do 
            --SDL.delay (1000 `div` 120)
            go keysDown' screen cses' gW'
        else do
            return ()
