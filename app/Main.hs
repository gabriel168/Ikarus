{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Data.List (unfoldr)
import Linear
import Control.Lens
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Color
import Text.Show.Functions

import Types
import Bodies
import Rendering

{---------------------------------------------------------
---------- The Comprehensive Control Manual --------------
----------------------------------------------------------
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
- Space: Fine Thrust for more precise manoeuvering
----------------------------------------------------------}

--Parameters
thrust = 100    --acceleration which the rocket is capable of
agility = 1+pi  --turning rate of the rocket
zoomSpeed = 6  

--Keypress detection - 
--add keys to the set if they are pressed, delete them if they are released 
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k -> parseEvents (delete k keysDown)
        _ -> parseEvents keysDown

--checks if a key k is pressed, i.e. contained in the set s
keyDown :: Set SDL.Keysym -> SDL.SDLKey -> Bool
keyDown s k = not . null . filter ((== k) . SDL.symKey) $ s

deriving instance Ord SDL.Keysym --Keys have to be ordered for the Data.Set data structure to work

--Key processing--
checkTimeWarp :: (SDL.SDLKey -> Bool) -> Double
checkTimeWarp keyDown' = 
       if keyDown' (SDL.SDLK_1) then 2
  else if keyDown' (SDL.SDLK_2) then 4
  else if keyDown' (SDL.SDLK_3) then 8
  else if keyDown' (SDL.SDLK_4) then 16
  else if keyDown' (SDL.SDLK_5) then 32
  else if keyDown' (SDL.SDLK_6) then 64
  else if keyDown' (SDL.SDLK_7) then 128
  else if keyDown' (SDL.SDLK_8) then 256
  else 1 

checkZoomRate :: (SDL.SDLKey -> Bool) -> Double
checkZoomRate keyDown' = 
       if keyDown' (SDL.SDLK_q) then zoomSpeed
  else if keyDown' (SDL.SDLK_e) then (-zoomSpeed)
  else 0

checkRotation :: (SDL.SDLKey -> Bool) -> Double -> (GameState -> Double)
checkRotation keyDown' dt =
    let --The WASD keys can be used to orient the rocket relative to its velocity 
        offset = if keyDown' (SDL.SDLK_w) then Just 0  
            else if keyDown' (SDL.SDLK_a) then Just (pi/2)
            else if keyDown' (SDL.SDLK_s) then Just pi
            else if keyDown' (SDL.SDLK_d) then Just (-pi/2)
            else Nothing
        --Left/Right arrow keys just turn the rocket
        turning_rate = if keyDown' (SDL.SDLK_LEFT)  then agility
                  else if keyDown' (SDL.SDLK_RIGHT) then (-agility)
                  else 0
    in case offset of
            Just x  -> (+x) . getA . vel . rocket
            Nothing -> (+(dt*turning_rate)) . orientation . rocket
        
checkThrust :: (SDL.SDLKey -> Bool) -> Double   
checkThrust keyDown' =  if keyDown' (SDL.SDLK_UP) then (
                            if keyDown' (SDL.SDLK_SPACE) then 0.1*thrust
                            else thrust ) 
                        else 0

--Core Logic

--Calculate the gravitational force from a body acting on a player
gravity :: V2 Double -> CelestialBody -> V2 Double
gravity player body = vec ^* ((mass body)/((**1.5).quadrance $ vec)) 
    where vec = (bodyPos body) - player

--Update the position of all the Planets
updateSystem :: Double -> [CelestialBody] -> [CelestialBody]
updateSystem time sys = map (\cb -> cb {bodyPos = orbit cb $ time }) sys

--Check wether the Player has crashed into a planet
isColliding :: V2 Double -> CelestialBody -> Bool
isColliding playerPos b = quadrance ((bodyPos b)-playerPos) < (size b)^2

--Compute the next Frame based on the previous one
nextFrame :: (HasTime a t) => t -> Set SDL.Keysym -> GameState -> GameState
nextFrame ds ks prevF = 
    let keyDown' = keyDown ks --key checking
        warpFactor = checkTimeWarp keyDown' --time Warp factor
        dt = (*warpFactor) . realToFrac . dtime $ ds --time step
        worldTime' = (+dt) . worldTime $ prevF --total time passed since start (includes timewarping)

        --View Zoom
        zoomRate = checkZoomRate keyDown'
        zoom' = if keyDown' (SDL.SDLK_r) then 1 --option to reset the zoom
                else (max 0.0005) . (min 15.0) $ ((*(1+dt*zoomRate)) . camZoom $ prevF) --limit the zoomFactor to prevent glitches 

        --Update the Solar System
        solarSystem' = updateSystem worldTime' (solarSystem prevF)

        --Ship Steering
        or' = checkRotation keyDown' dt $ prevF
        engine_Power = checkThrust keyDown' --In Thrust We Trust
        engine_acc = (^*engine_Power) . angle $ or'
        
        --Sum of all gravitational forces acting on the rocket
        gravt_acc = sum . map (gravity . pos . rocket $ prevF) $ solarSystem'
         
        --'Integrate' the next velocity/position
        acc' = gravt_acc + engine_acc
        vel' = (vel . rocket $ prevF) + (dt *^ acc')
        pos' = (pos . rocket $ prevF) + (dt *^ vel')
        
        -- Collision detection & ESC key  
        collided = any (isColliding pos') (solarSystem prevF) 
        stopRunning = keyDown' (SDL.SDLK_ESCAPE)            
                    || collided                       

    in if stopRunning then Over 
       else Running{ rocket = Rocket
                        { acc = acc'
                        , vel = vel'
                        , pos = pos'
                        , orientation = or' }
                   , camPos = pos' 
                   , camZoom = zoom'
                   , worldTime = worldTime'
                   , solarSystem = solarSystem'
                   , prediction = predict 10000 (0.2) prevF }

--Predict the positions of the rocket in the next n frames
predict :: Int -> Double -> GameState -> [V2 Double]
predict n dt g = take n $ unfoldr (\g -> Just ((pos . rocket $ g), predictFrame dt g)) g

--Similar to nextFrame except it ignores keystrokes & does not make another prediction within the prediction
predictFrame :: Double -> GameState -> GameState
predictFrame dt prevF =
    let worldTime' = (+dt) . worldTime $ prevF
        solarSystem' = updateSystem worldTime' (solarSystem prevF)

        gravt_acc = sum . map (gravity . pos . rocket $ prevF) $ solarSystem'

        acc' = gravt_acc
        vel' = (vel$rocket prevF) + (dt *^ acc')
        pos' = (pos$rocket prevF) + (dt *^ vel')

    in Running{ rocket = Rocket
                    { acc = acc'
                    , vel = vel'
                    , pos = pos'
                    , orientation = 0 }
                , camPos = V2 0 0
                , camZoom = 0
                , worldTime = worldTime'
                , solarSystem = solarSystem'
                , prediction = [] }

--------------------------------------------------------------
--The first frame
start :: GameState
start = Running
    { rocket = Rocket
        { acc = V2 0 0
        , vel = V2 0 (sqrt$(mass theSun)/((pos . rocket $ start)^._x)) --required velocity for a circular orbit around the sun
        , pos = V2 ((*1.1).size$theSun) 0 
        , orientation = 0 }
    , camPos = pos . rocket $ start
    , camZoom = 1   --0.7244633253307354 
    , worldTime = 0 
    , solarSystem = theSolarSystem
    , prediction = [] }

--Returns the current frame as well as an updated version of itself 
runGame :: (Monad m, HasTime t s) => GameState -> Wire s () m (Set SDL.Keysym) GameState
runGame prevF = mkPure $ \ds keys -> (Right prevF, runGame (nextFrame ds keys prevF))   

main :: IO ()
main = do
    --Open the Window:
    let width' = fromIntegral width
        height' = fromIntegral height 
    SDL.init [SDL.InitEverything]
    SDL.setCaption "Ikarus" ""
    screen <- SDL.setVideoMode width' height' 32 [SDL.SWSurface]
    
    void $ go empty screen clockSession_ (runGame start) --Start the game

    where
      go keysDown screen cses gW = do
        keysDown' <- parseEvents keysDown --check key presses/releases
        (ds, cses') <- stepSession cses   --update the clock session & get the time passed since the last frame
        (eg, gW') <- stepWire gW ds (Right keysDown') --calculate the next frame
        let ng = either (\_ -> start) id eg --extract the gamestate from the Either wrapper it was put in bec. thats how NetWire works
        --putStrLn . show $ ng 
        x <- renderFrame screen ng --renders everything, closes the window & returns false if the game has ended
        if x then do 
            go keysDown' screen cses' gW'
        else do
            return ()
