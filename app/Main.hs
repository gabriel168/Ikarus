{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Data.List (unfoldr, maximumBy)
import Linear
import Control.Lens hiding (view)
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Color
import Text.Show.Functions

import Types
import Bodies
import Rendering

--Parameters
thrust = 50     --acceleration which the rocket is capable of
agility = 3.5  --turning rate of the rocket
zoomSpeed = 5  

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

deriving instance Ord SDL.Keysym --Keys have to be ordered for the Set data structure to work

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
            Just x  -> (+x) . getA . relvel
                        where relvel = (\g -> (f . b $ g) - (f . a $ g))
                              b = (!!2) . prediction
                              a = (!!0) . prediction
                              f = \p -> (fst p) - (snd p)
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

--Check whether the Player has crashed into a planet
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
                else (max 0.0005) . (min 15.0) $ ((*(1+dt*zoomRate)) . camZoom . view $ prevF) --limit the zoomFactor to prevent glitches 

        --Ship Steering
        or' = checkRotation keyDown' dt $ prevF
        engine_Power = checkThrust keyDown' --In Thrust We Trust
        engine_acc = (^*engine_Power) . angle $ or'
 
        --Update the Solar System
        solarSystem' = updateSystem worldTime' (solarSystem prevF)
        
        --Gravity
        solSyswithGravity = map (\b -> ((gravity . pos . rocket $ prevF) b, b)) solarSystem'

        --Sum of all gravitational forces acting on the rocket
        gravt_acc = sum $ map fst solSyswithGravity

        --Find the position of the planet wich exerts the greatest gravitational force
        f = quadrance . fst
        soi_center = bodyPos . snd $ maximumBy (\a b -> compare (f a) (f b)) solSyswithGravity
        ignore_soi = if keyDown' (SDL.SDLK_f) then False
                     else if keyDown' (SDL.SDLK_g) then True
                     else ignoresoi prevF
        soi_center' = if ignore_soi then V2 0 0
                      else soi_center
  
        --'Integrate' the next velocity/position
        acc' = gravt_acc + engine_acc
        vel' = (vel . rocket $ prevF) + (dt *^ acc')
        pos' = (pos . rocket $ prevF) + (dt *^ vel')
        
        -- Collision detection & ESC key  
        collided = any (isColliding pos') (solarSystem prevF) 
        stopRunning = keyDown' (SDL.SDLK_ESCAPE)            
                    || collided                       

    in if stopRunning then Over 
       else prevF { rocket = Rocket
                        { acc = acc'
                        , vel = vel'
                        , pos = pos'
                        , orientation = or' }
                    , view = Camera
                        { camPos = pos' 
                        , camZoom = zoom' }
                    , worldTime = worldTime'
                    , solarSystem = solarSystem'
                    , prediction = predict 10000 0.3 (Just prevF) (ignore_soi)
                    , soicenter = soi_center'
                    , ignoresoi = ignore_soi }

--Predict the positions of the rocket in the next n frames
predict :: Int -> Double -> Maybe GameState -> Bool -> [(V2 Double, V2 Double)]
predict n dt g ignore_soi = take n $ (flip unfoldr) g 
                                  (\frame -> case frame of --note to future self: i am sorry
                                            Nothing -> Nothing
                                            Just frame -> Just (
                                                (
                                                    (pos . rocket $ frame),
                                                    (if ignore_soi then V2 0 0 else soicenter frame)
                                                ) 
                                                , predictFrame dt frame)
                                  )

--Similar to nextFrame except it ignores keystrokes & does not make another prediction within the prediction
predictFrame :: Double -> GameState -> Maybe GameState
predictFrame dt prevF =
    let worldTime' = (+dt) . worldTime $ prevF
        solarSystem' = updateSystem worldTime' (solarSystem prevF)
        
        --Gravity
        solSyswithGravity = map (\b -> ((gravity . pos . rocket $ prevF) b, b)) solarSystem' 
        
        --Sum of all gravitational forces acting on the rocket
        gravt_acc = sum $ map fst solSyswithGravity
        
        --Find the position of the planet wich exerts the greatest gravitational force
        f = quadrance . fst
        soi_center = bodyPos . snd $ maximumBy (\a b -> compare (f a) (f b)) solSyswithGravity        

        acc' = gravt_acc
        vel' = (vel$rocket prevF) + (dt *^ acc')
        pos' = (pos$rocket prevF) + (dt *^ vel')
  
        collided = any (isColliding pos') (solarSystem')

    in if collided then Nothing
       else Just prevF { rocket = Rocket
                    { acc = acc'
                    , vel = vel'
                    , pos = pos'
                    , orientation = 0 }
                , view = Camera
                    { camPos = V2 0 0
                    , camZoom = 0 }
                , worldTime = worldTime'
                , solarSystem = solarSystem'
                , prediction = []
                , soicenter = soi_center }

--------------------------------------------------------------
--The first frame
start :: GameState
start = Running
    { rocket = Rocket
        { acc = V2 0 0
        , vel = V2 0 (sqrt $ (mass theSun)/((pos . rocket $ start)^._x) ) --required velocity for a circular orbit around the sun
        , pos = V2 ((*1.1).size$theSun) 0 
        , orientation = 0 }
    , view = Camera
        { camPos = V2 0 0 
        , camZoom = 1 {-0.7244633253307354-} }
    , worldTime = 0 
    , solarSystem = theSolarSystem
    , prediction = []
    , soicenter = V2 0 0 
    , ignoresoi = False }

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
        x <- renderFrame screen ng --renders everything, closes the window & returns false if the game has ended
        --putStrLn . show $ ds
        if x then do 
            go keysDown' screen cses' gW'
        else do
            return ()
