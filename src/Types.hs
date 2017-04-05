module Types where

import qualified Graphics.UI.SDL.Color as SDL
import Linear

data Rocket = Rocket
    { acc :: V2 Double
    , vel :: V2 Double
    , pos :: V2 Double
    , orientation :: Double }

data GameState = Running
    { rocket :: !Rocket 
    , camPos :: (V2 Double)
    , camZoom :: !Double
    , worldTime :: !Double
    , solarSystem :: [CelestialBody] }
               | Over

data CelestialBody = Body
    { size :: !Double
    , mass :: !Double
    , orbitRadius :: !Double
    , orbit :: Double -> V2 Double
    , bodyPos :: !(V2 Double)
    , colour :: !SDL.Pixel }

