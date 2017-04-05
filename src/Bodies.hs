module Bodies where

import Linear
import qualified Graphics.UI.SDL as SDL 
import Graphics.UI.SDL.Color
import Text.Show.Functions

angVel :: Double -> Double -> Double
angVel m r = sqrt$ m/(r^3)

angVel' = angVel$mass theSun

getOrbit :: Double -> (Double -> V2 Double)
getOrbit t_r = \x ->  (^*(t_r)) . angle . (*(angVel'$t_r)) $ x

data CelestialBody = Body
    { size :: !Double
    , mass :: !Double
    , trajectoryRadius :: !Double
    , trajectory :: Double -> V2 Double 
    , bodyPos :: !(V2 Double)
    , colour :: !SDL.Pixel }

theSolarSystem :: [CelestialBody]
theSolarSystem = [theSun, merkur, venus]

theSun :: CelestialBody
theSun = Body
    { mass = 1000000000
    , size = 10000
    , trajectoryRadius = 0
    , trajectory = \_ -> V2 0 0
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xFFDE00FF }

merkur :: CelestialBody
merkur = Body
    { mass = 5000000
    , size = 1000
    , trajectoryRadius = 50000
    , trajectory = getOrbit$trajectoryRadius merkur
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xB8434FF }

venus :: CelestialBody
venus = Body
    { mass = 6000000
    , size = 1700
    , trajectoryRadius = 100000
    , trajectory = getOrbit$trajectoryRadius venus
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xFF0000FF }
