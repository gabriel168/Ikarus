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
- Number n from 1-6: Warp time by a factor of 2^n
-
--Steering--
- W/S: Point Rocket Pro-/Retrograde
- A/D: Point Rocket Radially In/Out
- Left/Right: Turn Rocket
- Up: ACTIVATE ENGINE
------------------------------------}

--Parameters
width = 1280 
height = 720
thrust = 100 
agility = 1+pi
zoomSpeed = 10

solarMass = 1000000000
solarRadius = 10000

-- Planet Coordinates
center = V2 xP' yP' 
xP' = fromIntegral $ div width 2
yP' = fromIntegral $ div height 2

xP = 0
yP = 0
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

--Key processing
getWarp :: Set SDL.Keysym -> Double
getWarp ks = 
    let keyDown = flip keyDown' $ ks
    in 
       if keyDown (SDL.SDLK_1) then 2
  else if keyDown (SDL.SDLK_2) then 4
  else if keyDown (SDL.SDLK_3) then 8
  else if keyDown (SDL.SDLK_4) then 16
  else if keyDown (SDL.SDLK_5) then 32
  else if keyDown (SDL.SDLK_6) then 64
  else 1 

getZoom  :: Set SDL.Keysym -> Double
getZoom ks = 
    let  keyDown = flip keyDown' $ ks
    in 
            if keyDown (SDL.SDLK_q) then zoomSpeed
       else if keyDown (SDL.SDLK_e) then (-zoomSpeed)
       else 0

getOrientation :: Set SDL.Keysym -> Double -> (GameState -> Double)
getOrientation ks dt =
    let  keyDown = flip keyDown' $ ks
         offset  = if keyDown (SDL.SDLK_w) then Just 0  
              else if keyDown (SDL.SDLK_s) then Just pi
              else if keyDown (SDL.SDLK_a) then Just (pi/2)
              else if keyDown (SDL.SDLK_d) then Just (-pi/2)
              else Nothing 
         turning_rate = if keyDown (SDL.SDLK_LEFT)  then agility
                   else if keyDown (SDL.SDLK_RIGHT) then (-agility)
                   else 0
    in case offset of
            Just x  -> (+x) . getA . vel
            Nothing -> (+(dt*turning_rate)) . orientation 
        
 
--Core Logic
data CelestialBody = Body
    { radius :: !Double
    , mass :: !Double
    , trajectory :: Double -> V2 Double 
    , bodyPos :: (V2 Double)
    , colour :: !SDL.Pixel }
                   deriving (Show)

data GameState = Running
    { acc :: (V2 Double)
    , vel :: (V2 Double)
    , pos :: (V2 Double) 
    , orientation :: !Double
    , camPos :: (V2 Double)
    , camZoom :: !Double 
    , worldTime :: !Double }
               | Over
               deriving (Show)
start :: GameState
start = Running
    { acc = V2 0 0
    , vel = V2 0 (-313)
    , pos = V2 (-10220) 0 
    , orientation = 0
    , camPos = pos start
    , camZoom = 1 
    , worldTime = 0 }

runGame :: (Monad m, HasTime t s) => GameState -> Wire s () m (Set SDL.Keysym) GameState
runGame prevF = mkPure $ \ds keys -> (Right prevF, runGame (nextFrame ds keys prevF))   

nextFrame :: (HasTime a t) => t -> Set SDL.Keysym -> GameState -> GameState
nextFrame ds ks prevF  = 
    let keyDown = flip keyDown' $ ks
        warpF = getWarp ks
        dt = (*warpF) . realToFrac . dtime $ ds
        
        --View Zoom
        zoomRate = getZoom ks
        zoom' = if keyDown (SDL.SDLK_r) then 1
                else (*(1+dt*zoomRate)) . camZoom $ prevF
        
        or' = getOrientation ks dt $ prevF
        --Gravity
        pVec = (V2 xP yP) - (pos prevF)  
        gravity = pVec ^* (solarMass/((quadrance pVec)**1.5))
        
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
                   , camZoom = zoom'
                   , worldTime = dt+(worldTime prevF) }

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

relVecCoordinates' :: (Num a) => a -> a -> V2 a -> (a,a)
relVecCoordinates' z f v = (z*f*v^._x, -z*f*v^._y)

renderBody :: SDL.Surface -> CelestialBody -> IO ()
renderBody screen body = do
    -- TODO
    return ()


renderFrame :: SDL.Surface -> GameState -> IO Bool
renderFrame screen Over = do 
    SDL.quit
    return False
renderFrame screen game = do
    let toCrapCoordinates = toCrapCoordinates' (camPos game) (camZoom game)
        relVecCoordinates = relVecCoordinates' $ camZoom game
        zF = camZoom game
    --Background
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
   
    --Planet
    let (xP', yP') = toCrapCoordinates (V2 xP yP) 
    filledCircle screen (round xP') (round yP') (round$solarRadius*zF) (SDL.Pixel 0xFFDE00FF)
    
    --Rocket
    let (x,y) = toCrapCoordinates . pos $ game
        pI = 0.7*pi
        a = angle . orientation $ game 
        (a_x,a_y) = relVecCoordinates 30 a
        b = angle .(+pI) . orientation $ game
        (b_x,b_y) = relVecCoordinates 20 b
        c = angle . (+(-pI)) . orientation $ game
        (c_x,c_y) = relVecCoordinates 20 c
    
    filledTrigon screen (round$x+a_x) (round$y+a_y) (round$x+b_x) (round$y+b_y) (round$x+c_x) (round$y+c_y) (SDL.Pixel 0xC4CED3FF) 
    filledCircle screen (round x) (round y) (2) (SDL.Pixel 0xC4CED3FF)
    
    --Velocity Indicator
    let v = vel game
        (vel_x,vel_y) = relVecCoordinates 1 v  
        indS = round $ zF * 15
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 40 50 >>= SDL.fillRect screen (Just $ SDL.Rect (round$x-zF*10+vel_x) (round$y-zF*10+vel_y) indS indS)  
    
    --Acceleration Indicator
    let a = acc game
        (acc_x,acc_y) = relVecCoordinates 1 a
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 40 250 40 >>= SDL.fillRect screen (Just $ SDL.Rect (round$x-zF*10+acc_x) (round$y-zF*10+acc_y) indS indS)

    SDL.flip screen
    return True


--------------------------------------------------------------

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.setCaption "Ikarus" ""
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    void $ go empty screen clockSession_ gameW

    where 
    go keysDown screen cses gW = do
        keysDown' <- parseEvents keysDown
        (ds, cses') <- stepSession cses 
        (eg, gW') <- stepWire gW ds (Right keysDown')
        let ng = either (const start) id eg
        
        x <- renderFrame screen ng  
        if x then do 
            SDL.delay (1000 `div` 120)
            go keysDown' screen cses' gW'
        else do
            return ()
