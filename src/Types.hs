module Types where

import qualified Graphics.UI.SDL.Color as SDL
import Linear

--Represents the player's rocket. The mass is implicitly set to one
data Rocket = Rocket
    { acc :: V2 Double
    , vel :: V2 Double
    , pos :: V2 Double
    , orientation :: Double }

--Represents a Sun or a Planet.
data CelestialBody = Body
    { size :: !Double
    , mass :: !Double
    , orbitRadius :: !Double
    , orbit :: Double -> V2 Double --Fixed orbits representing the position as a function of time
    , bodyPos :: !(V2 Double)      --Stores the position to avoid calculating it more often than necessary
    , colour :: !SDL.Pixel }

--Represent the state of the entire simulation
data GameState = Running
    { rocket :: !Rocket 
    , camPos :: (V2 Double)
    , camZoom :: !Double
    , worldTime :: !Double
    , solarSystem :: [CelestialBody]
    , prediction :: ![V2 Double] }
               | Over 
