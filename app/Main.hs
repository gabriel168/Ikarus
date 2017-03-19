{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-} --forall
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Linear
import Control.Lens
import qualified Graphics.UI.SDL as SDL

--Parameters
width = 1280 
height = 720
thrust = 80.0
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

keyDown :: SDL.SDLKey -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.symKey)

deriving instance Ord SDL.Keysym

--Core Logic
data GameState = GameState{
    rocket_Acc :: V2 Double,
    rocket_Vel :: V2 Double,
    rocket_Pos :: V2 Double, 
    rocket_Orientation :: Double
}

start :: GameState
start = GameState{
    rocket_Acc = V2 0 0,
    rocket_Vel = V2 0 40,
    rocket_Pos = V2 100 100,
    rocket_Orientation = 0
}

runGame :: (Monad m, HasTime t s) => GameState -> Wire s () m (Set SDL.Keysym) GameState
runGame prevF = mkPure $ \ds ks -> (Right prevF, runGame (next ds ks))   
    where next ds ks = let dtv = realToFrac (dtime ds) :: V2 Double
                           dts = realToFrac (dtime ds) :: Double
                           pressed = flip keyDown $ ks
                           gravity = V2 0 0
                           engine_acc = (*engine_Power) . angle . rocket_Orientation $ prevF
                           engine_Power = if pressed (SDL.SDLK_UP) then thrust 
                                          else if pressed (SDL.SDLK_DOWN) then (-thrust)
                                          else 0
                           omega = if pressed (SDL.SDLK_LEFT) then agility
                                   else if pressed (SDL.SDLK_RIGHT) then (-agility)
                                   else 0
                     in GameState{
                            rocket_Acc = engine_acc + gravity,
                            rocket_Vel = rocket_Vel prevF + (dtv*rocket_Acc prevF),
                            rocket_Pos = rocket_Pos prevF + (dtv*rocket_Vel prevF),
                            rocket_Orientation = rocket_Orientation prevF + dts*omega
                        }

gameW = runGame start
        
--Graphics Rendering
toCrapCoordinates :: (Num a) => (a,a) -> (a,a)
toCrapCoordinates (x,y) = (x,(fromIntegral height)-y)

render :: SDL.Surface -> GameState -> IO ()
render screen game = do
    --Background
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
    
    --Rocket
    let p = rocket_Pos game
        (x,y) = toCrapCoordinates (p ^._x, p ^._y)
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x-25) (round (y-25)) 50 50)
    --Orientation Indicator
    let  dir = angle . rocket_Orientation $ game
         (dir_x, dir_y) = (f*(dir ^._x), -f*(dir ^._y)) where f = 10.0 :: Double
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 200 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+dir_x)) (round (y-10+dir_y)) 20 20)
    --Velocity Indicator
    let v = rocket_Vel game
        (vel_x, vel_y) = (f*(v ^._x), -f*(v^._y)) where f = 1
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 0 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+vel_x)) (round (y-10+vel_y)) 20 20)  
    
    SDL.flip screen

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
        render screen ng  
          
        SDL.delay (1000 `div` 60)
        go keysDown' screen cses' gW'              

