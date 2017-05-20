module Bodies where

import Linear
import qualified Graphics.UI.SDL as SDL 
import Graphics.UI.SDL.Color
import Text.Show.Functions

import Types

--Angular velocity of an body in circular orbit (radius r) around a body of mass m
angVel :: Double -> Double -> Double
angVel m r = sqrt $ m/(r^3)

--Angular velocity of a planet orbiting our sun at a certain radius
angVel' = angVel $ mass theSun

--Returns the orbit function for a given body with orbit radius t_r
getOrbit :: Double -> (Double -> V2 Double)
getOrbit t_r = \x ->  (^*(t_r)) . angle . (*(angVel'$t_r)) $ x

----------------------------------------------------------------

theSolarSystem :: [CelestialBody]
theSolarSystem = [theSun, merkur, venus]

theSun :: CelestialBody
theSun = Body
    { mass = 1000000000
    , size = 10000
    , orbitRadius = 0
    , orbit = \_ -> V2 0 0
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xFFDE00FF }

merkur :: CelestialBody
merkur = Body
    { mass = 5000000
    , size = 1000
    , orbitRadius = 50000
    , orbit = getOrbit$orbitRadius merkur
    , bodyPos = V2 0 0 
    , colour = SDL.Pixel 0xB8434FF }

venus :: CelestialBody
venus = Body
    { mass = 6000000
    , size = 1700
    , orbitRadius = 100000
    , orbit = getOrbit $ orbitRadius venus
    , bodyPos = V2 0 0
    , colour = SDL.Pixel 0xFF0000FF }
