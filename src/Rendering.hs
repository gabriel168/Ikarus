module Rendering where

import Linear
import Control.Lens
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Color

import Types

width = 1920 
height = 1000

getA :: V2 Double -> Double
getA v = atan2 y x
    where x = v ^._x
          y = v ^._y

center = V2 (f width) (f height)
    where f = fromIntegral . (`div` 2)

toCrapCoordinates' :: V2 Double -> Double -> V2 Double -> (Double, Double)
toCrapCoordinates' camP z v = (dispV ^._x, (height' - dispV ^._y))
    where dispV = z*^(v - camP) + center
          height' = fromIntegral height

relVecCoordinates' :: (Num a) => a -> a -> V2 a -> (a,a)
relVecCoordinates' z f v = (z*f*v^._x, -z*f*v^._y)

-------------------------------------------------------

renderRocket :: SDL.Surface -> Rocket 
    -> (V2 Double -> (Double, Double))              
    -> (Double -> V2 Double -> (Double, Double)) 
    -> Double
    -> IO()
renderRocket screen rocket' tcC rvC zF = do
    let (x,y) = tcC . pos $ rocket'
        pI = 0.7*pi
        a = angle . orientation $ rocket' 
        (a_x,a_y) = rvC 30 a
        b = angle .(+pI) . orientation $ rocket'
        (b_x,b_y) = rvC 20 b
        c = angle . (+(-pI)) . orientation $ rocket'
        (c_x,c_y) = rvC 20 c
    
    filledTrigon screen (round$x+a_x) (round$y+a_y) (round$x+b_x) (round$y+b_y) (round$x+c_x) (round$y+c_y) (SDL.Pixel 0xC4CED3FF) 
    filledCircle screen (round x) (round y) (2) (SDL.Pixel 0xC4CED3FF)
    
    --Velocity Indicator
    let v = vel rocket'
        (vel_x,vel_y) = rvC 1 v  
        indS = round $ zF * 15
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 40 50 >>= SDL.fillRect screen (Just $ SDL.Rect (round$x-zF*10+vel_x) (round$y-zF*10+vel_y) indS indS)  
    
    --Acceleration Indicator
    let a = acc rocket'
        (acc_x,acc_y) = rvC 1 a
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 40 250 40 >>= SDL.fillRect screen (Just $ SDL.Rect (round$x-zF*10+acc_x) (round$y-zF*10+acc_y) indS indS)
    return ()

---------------------------------------------------------    
    
renderBody :: SDL.Surface -> (V2 Double -> (Double, Double)) -> Double -> CelestialBody -> IO ()
renderBody screen tcC zF body = do
    let (xC, yC) = tcC $ V2 0 0
        (x,y) = tcC $ bodyPos body
        r = round $ (*zF) . size $ body
        t_r = orbitRadius body
    circle screen (round xC) (round yC) (round$zF*t_r) (SDL.Pixel 0x33B5E5AA)    
    filledCircle screen (round x) (round y) r (colour body)
    return ()

--------------------------------------------------------

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
   
    --Planets
    mapM_ (renderBody screen toCrapCoordinates zF) (solarSystem game)

    --Rocket
    renderRocket screen (rocket game) toCrapCoordinates relVecCoordinates zF
    
    SDL.flip screen
    return True
