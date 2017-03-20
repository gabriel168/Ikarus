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

--Parameters
width = 1280 
height = 720
thrust = 100 :: Double
agility = 1+pi

-- Planet Coordinates
xP = fromIntegral $ div width 2
yP = fromIntegral $ div height 2

--Keypress detection
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k -> parseEvents (delete k keysDown)
        _ -> parseEvents keysDown

keyDown' :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown' k = not . null . filter ((== k) . SDL.symKey) 

deriving instance Ord SDL.Keysym

--Core Logic
data GameState = GameState{
      acc :: V2 Double
    , vel :: V2 Double
    , pos :: V2 Double 
    , orientation :: Double
} | Over

start :: GameState
start = GameState{
      acc = V2 0 0
    , vel = V2 0 (-200)
    , pos = V2 (xP-220) yP
    , orientation = 0
}

runGame :: (Monad m, HasTime t s) => GameState -> Wire s () m (Set SDL.Keysym) GameState
runGame prevF = mkPure $ \ds keys -> (Right prevF, runGame (next ds keys))   
    where next ds ks = let keyDown = flip keyDown' $ ks
                           dt = realToFrac (dtime ds)
                           
                           --Turn Rocket
                           turning_rate = if keyDown (SDL.SDLK_LEFT) then agility
                                   else if keyDown (SDL.SDLK_RIGHT) then (-agility)
                                   else 0
                           
                           --Prograde/Retrograde hold
                           or' = if keyDown (SDL.SDLK_w) then getA $ vel prevF
                                 else if keyDown (SDL.SDLK_s) then (+pi).getA $ vel prevF
                                 else if keyDown (SDL.SDLK_a) then (+pi/2).getA $ vel prevF
                                 else if keyDown (SDL.SDLK_d) then (+(-pi/2)).getA $ vel prevF
                                 else (+(dt*turning_rate)) . orientation $ prevF
                           
                           --Gravity
                           pVec = (V2 xP yP) - (pos prevF)  
                           gravity = pVec ^* (10000000/((quadrance pVec)**1.5))
                           -- In Thrust We Trust --
                           engine_acc = (^*engine_Power) . angle $ or'
                           engine_Power = if keyDown (SDL.SDLK_UP) then thrust 
                                          else if keyDown (SDL.SDLK_DOWN) then (-thrust)
                                          else 0
                            
                           acc' = engine_acc + gravity
                           vel' = vel prevF + (dt *^ acc')
                           pos' = pos prevF + (dt *^ vel')

                        in if keyDown (SDL.SDLK_q) then Over 
                           else GameState{  acc = acc'
                                           , vel = vel'
                                           , pos = pos'
                                           , orientation = or' }

gameW :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) GameState
gameW = runGame start

getA :: V2 Double -> Double
getA v = atan2 y x
    where x = v ^._x
          y = v ^._y

--Graphics Rendering
toCrapCoordinates :: (Num a) => (a,a) -> (a,a)
toCrapCoordinates (x,y) = (x,(fromIntegral height)-y)

render :: SDL.Surface -> GameState -> IO Bool
render screen Over = do 
    SDL.quit
    return False
render screen game = do
    --Background
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
   
    --Planet
    filledCircle screen (round xP) (round yP) 120 (SDL.Pixel 0xE59623FF)   
    
    --Rocket
    let p = pos game
        (x,y) = toCrapCoordinates (p ^._x, p ^._y)
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x-25) (round (y-25)) 50 50)

    --Orientation Indicator
    let  dir = angle . orientation $ game
         (dir_x, dir_y) = (f*(dir ^._x), -f*(dir ^._y)) where f = 15.0 :: Double
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 200 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+dir_x)) (round (y-10+dir_y)) 20 20)

    --Velocity Indicator
    let v = vel game
        (vel_x, vel_y) = (f*(v ^._x), -f*(v^._y)) where f = 1
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 0 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+vel_x)) (round (y-10+vel_y)) 20 20)  
    
    --Acceleration Indicator
    let a = acc game
        (a_x, a_y) = (f*(a^._x), -f*(a^._y)) where f = 1
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 20 100 100 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+a_x)) (round (y-10+a_y)) 20 20)

    SDL.flip screen
    return True

--------------------------------------------------------------

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setCaption "Ikarus" ""
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    go empty screen clockSession_ gameW

    where 
    go keysDown screen cses gW = do
        keysDown' <- parseEvents keysDown
        (ds, cses') <- stepSession cses 
        (eg, gW') <- stepWire gW ds (Right keysDown')
        let ng = either (const start) id eg
        
        x <- render screen ng  
        if x then do 
            SDL.delay (1000 `div` 60)
            go keysDown' screen cses' gW'
        else do
            return ()
