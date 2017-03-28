module Bodies where

import Linear
import qualified Graphics.UI.SDL as SDL 
import Graphics.UI.SDL.Color
import Text.Show.Functions


data CelestialBody = Body
    { size :: !Double
    , mass :: !Double
    , trajectoryRadius :: !Double
    , trajectory :: Double -> V2 Double 
    , bodyPos :: !(V2 Double)
    , colour :: !SDL.Pixel }

theSolarSystem :: [CelestialBody]
theSolarSystem = [theSun, thePlanet]

theSun :: CelestialBody
theSun = Body
    { mass = 1000000000
    , size = 10000
    , trajectoryRadius = 0
    , trajectory = \_ -> V2 0 0
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xFFDE00FF }

thePlanet :: CelestialBody
thePlanet = Body
    { mass = 5000000
    , size = 1000
    , trajectoryRadius = 30000
    , trajectory = \x -> (^*(trajectoryRadius thePlanet)) . angle . (*0.01) $ x
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xB8434FF }
