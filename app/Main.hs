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
thrust = 100 
agility = 1+pi
zoomSpeed = 10

-- Planet Coordinates
center = V2 xP yP 
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

data GameState = Running{
      acc :: V2 Double
    , vel :: V2 Double
    , pos :: V2 Double 
    , orientation :: !Double
    , camPos :: V2 Double
    , camZoom :: !Double }
               | Over

start :: GameState
start = Running{
      acc = V2 0 0
    , vel = V2 0 (-210)
    , pos = V2 (xP-220) yP
    , orientation = 0
    , camPos = center
    , camZoom = 1 }

runGame :: (Monad m, HasTime t s) => GameState -> Wire s () m (Set SDL.Keysym) GameState
runGame prevF = mkPure $ \ds keys -> (Right prevF, runGame (nextFrame ds keys prevF))   
    
nextFrame :: (HasTime a t) => t -> Set SDL.Keysym -> GameState -> GameState
nextFrame ds ks prevF  = 
    let keyDown = flip keyDown' $ ks
        dt = realToFrac (dtime ds)
        
        --View Zoom
        zoomRate = if keyDown (SDL.SDLK_q) then zoomSpeed
                 else if keyDown (SDL.SDLK_e) then (-zoomSpeed)
                 else 0
        zoom' = (*(1+dt*zoomRate)) . camZoom $ prevF
        --Cam Pos
        camPos' = (dt*^camDir) + (camPos prevF)
        cS = 100
        camDir = if keyDown (SDL.SDLK_i) then V2 0 cS
                 else if keyDown (SDL.SDLK_j) then V2 (-cS) 0
                 else if keyDown (SDL.SDLK_k) then V2 0 (-cS)
                 else if keyDown (SDL.SDLK_l) then V2 cS 0
                 else V2 0 0
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
        pVec = (center) - (pos prevF)  
        gravity = pVec ^* (10000000/((quadrance pVec)**1.5))
        -- In Thrust We Trust --
        engine_acc = (^*engine_Power) . angle $ or'
        engine_Power = if keyDown (SDL.SDLK_UP) then thrust 
                       else 0
         
        acc' = engine_acc + gravity
        vel' = vel prevF + (dt *^ acc')
        pos' = pos prevF + (dt *^ vel')

    in if keyDown (SDL.SDLK_ESCAPE) then Over 
       else Running{ acc = acc'
                   , vel = vel'
                   , pos = pos'
                   , orientation = or'
                   , camPos = pos' 
                   , camZoom = zoom' }

gameW :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) GameState
gameW = runGame start

getA :: V2 Double -> Double
getA v = atan2 y x
    where x = v ^._x
          y = v ^._y

--Graphics Rendering
toCrapCoordinates' :: V2 Double -> Double -> V2 Double -> (Double, Double)
toCrapCoordinates' camP z v = (dispV ^._x, (height' - dispV ^._y))
    where dispV = z*^(v - camP) + center
          height' = fromIntegral height

relVecCoordinates' :: (Num a) => a -> V2 a -> a -> (a,a)
relVecCoordinates' z v f = (z*f*v^._x, -z*f*v^._y)

render :: SDL.Surface -> GameState -> IO Bool
render screen Over = do 
    SDL.quit
    return False

render screen game = do
    let toCrapCoordinates = toCrapCoordinates' (camPos game) (camZoom game)
        relVecCoordinates = relVecCoordinates' $ camZoom game
        zF = camZoom game
    --Background
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
   
    --Planet
    let (xP', yP') = toCrapCoordinates center 
    filledCircle screen (round xP') (round yP') (round$120*zF) (SDL.Pixel 0xAE6826FF)
    
    --Rocket
    let (x,y) = toCrapCoordinates . pos $ game
        pI = 0.7*pi
        a = angle . orientation $ game 
        (a_x,a_y) = relVecCoordinates a 30 
        b = angle .(+pI) . orientation $ game
        (b_x,b_y) = relVecCoordinates b 20
        c = angle . (+(-pI)) . orientation $ game
        (c_x,c_y) = relVecCoordinates c 20
    
    filledTrigon screen (round$x+a_x) (round$y+a_y) (round$x+b_x) (round$y+b_y) (round$x+c_x) (round$y+c_y) (SDL.Pixel 0xC4CED3FF) 
    --filledCircle screen (round x) (round y) (3) (SDL.Pixel 0xFFFFFFFF)
    
    --Velocity Indicator
    let v = vel game
        (vel_x,vel_y) = relVecCoordinates v 1 
        indS = round $ zF * 15
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 40 50 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+vel_x)) (round (y-10+vel_y)) indS indS)  
    
    --Acceleration Indicator
    let a = acc game
        (acc_x,acc_y) = relVecCoordinates a 1
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 40 250 40 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+acc_x)) (round (y-10+acc_y)) indS indS)

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
