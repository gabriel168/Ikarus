module Rendering where

import Linear
import Control.Lens hiding (view)
import Control.Monad
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Graphics.UI.SDL.Color

import Types

--Window dimensions in Pixels
width = 1920 
height = 1000

--Checks wether a point is in the window
pointOnScreen :: (Ord a, Num a) => (a, a) -> Bool
pointOnScreen (x,y) = not $ (x > width') || (y > height') || (x < 0) || (y < 0)
    where width' = fromIntegral width
          height' = fromIntegral height

--Check wether a planet is in the window
planetOnScreen :: (Ord a, Num a) => (a,a) -> a -> Bool
planetOnScreen (x,y) r = (x < width') || (y < height') || (x > (-r)) || (y > (-r))
    where width' = (+r) . fromIntegral $ width
          height' = (+r) . fromIntegral $ height

--returns the angle in which a vector is pointing
getA :: V2 Double -> Double
getA v = atan2 y x
    where x = v ^._x
          y = v ^._y

center = V2 (f width) (f height)
    where f = fromIntegral . (`div` 2)

--Converts points from our coordinate system (i.e. origin at the center of the sun)
--to where they need to be displayed in the window (origin at top left corner, y axis oriented downwards)
--camera position & zoom are accounted for         
toWindowCoordinates' :: CameraView -> V2 Double -> (Double, Double) 
toWindowCoordinates' cam point = (x, y)
    where (x, y) = (dispV ^._x, (height' - dispV ^._y))
          dispV = (camZoom cam)*^(point - (camPos cam)) + center
          height' = fromIntegral height

--Scales a vector v by both the z and f, returns it as pair
relVecCoordinates' :: (Num a) => a -> a -> V2 a -> (a,a) 
relVecCoordinates' z f v = (z*f*v^._x, -z*f*v^._y)

-------------------------------------------------------

renderRocket :: SDL.Surface -> Rocket --window & rocket to be rendered on the window
    -> (V2 Double -> (Double, Double)) --toWindowCoordinates function             
    -> (Double -> V2 Double -> (Double, Double)) -- relVecCoordinates Function
    -> IO()
renderRocket screen rocket' tWC rvC = do
    let (x,y) = tWC . pos $ rocket'
        pI = 0.7*pi
        a = angle . orientation $ rocket' 
        (a_x,a_y) = rvC 30 a
        b = angle .(+pI) . orientation $ rocket'
        (b_x,b_y) = rvC 20 b
        c = angle . (+(-pI)) . orientation $ rocket'
        (c_x,c_y) = rvC 20 c
    
    --Displays the rocket as pointy triangle:
    filledTrigon screen (round $ x+a_x) (round $ y+a_y) (round $ x+b_x) (round $ y+b_y) (round $ x+c_x) (round $ y+c_y) (SDL.Pixel 0xC4CED3FF) 
    
    --tiny, fixed size circle to have the rocket still be visible when zoomed out really far:
    filledCircle screen (round x) (round y) (2) (SDL.Pixel 0xC4CED3FF) 
    
    return ()

---------------------------------------------------------    
    
renderBody :: SDL.Surface 
    -> (V2 Double -> (Double, Double)) --toWindowCoordinates function
    -> Double --zoom Factor
    -> CelestialBody -> IO () --Body to be rendered
renderBody screen tWC zF body = do
    let (xC, yC) = tWC $ V2 0 0
        (x,y) = tWC $ bodyPos body
        r = (*zF) . size $ body
        t_r = orbitRadius body
        onScr = planetOnScreen (xC, yC) r
    
    --circle indicating the planets orbit:
    circle screen (round xC) (round yC) (round $ zF*t_r) (SDL.Pixel 0xFFFFFFAA) 
    
    --planet rendered only when on screen
    if onScr then filledCircle screen (round x) (round y) (round r) (colour body) 
             else return True
    return ()

--------------------------------------------------------
--connect the predicted positions to a line
renderPrediction :: SDL.Surface -> V2 Double -> (V2 Double -> (Double, Double)) -> [(V2 Double, V2 Double)] -> IO ()
renderPrediction screen soi_center tWC pts = do
    zipWithM (connectPoints screen soi_center tWC (SDL.Pixel 0x6071AF)) pts (tail pts)
    return ()

--make a line between two points if they are both within the window    
connectPoints :: SDL.Surface -> V2 Double -> (V2 Double -> (Double, Double)) -> SDL.Pixel -> (V2 Double, V2 Double) -> (V2 Double, V2 Double) -> IO ()
connectPoints surface soicenter tWC c a b = do 
    let r = round
        f (rocket, planet) = (x_rel, y_rel)
            where (x_rel, y_rel) = tWC $ soicenter + rocket - planet

        (xa, ya) = f a
        (xb, yb) = f b
        onScr = (pointOnScreen (xa,ya)) || (pointOnScreen (xb, yb))
    if onScr 
       then line surface (r xa) (r ya) (r xb) (r yb) c
       else return True 
    return ()

--------------------------------------------------------

renderFrame :: SDL.Surface -> GameState -> IO Bool
renderFrame screen Over = do
    SDL.quit --close the window if the game is over
    return False
renderFrame screen game = do
    let toWindowCoordinates = toWindowCoordinates' (view game)
        relVecCoordinates = relVecCoordinates' $ camZoom . view $ game
        zF = camZoom . view $ game
        
    --Background
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
   
    --Planets
    mapM_ (renderBody screen toWindowCoordinates zF) (solarSystem game)

    --Rocket
    renderRocket screen (rocket game) toWindowCoordinates relVecCoordinates 

    --Prediction
    renderPrediction screen (soicenter game) toWindowCoordinates (prediction game)

    SDL.flip screen
    return True
