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

{- Geplant:
data Parameters = Parameters{
    screenW :: forall a. Num a => a,
    screenH :: forall a. Num a => a,
    engine_POWER :: Double,
    startPos :: V2 Double
}

defaultParams :: Parameters
defaultParams = Parameters{
    screenW = 1280,
    screenH = 720,
    engine_POWER = 40.0,
    startPos = V2 100 100
}

data GameState = GameState{
    rocket_Acc :: V2 Double,
    rocket_Vel :: V2 Double,
    rocket_Pos :: forall m. forall t. forall s. (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double),
    rocket_Orientation :: Double
}

render :: SDL.Surface -> GameState -> IO ()
render screen game = do
    let p = rocket_Pos game
        (x,y) = toCrapCoordinates (p ^._x, p ^._y)
        dir = angle . rocket_Orientation $ game
        (dir_x,dir_y) = (f*(dir ^._x), f*(dir ^._y)) where f = 10.0 :: Double

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x-25) (round (y-25)) 50 50)
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 200 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-15+dir_x)) (round (y-15+dir_y)) 30 30)
    return ()
-}

----------------------------------------------------
-- Provisorisch:
width = 1280 
height = 720
thrust = 60.0
agility = pi

-- Planet Coordinates
xP = fromIntegral $ div width 2
yP = fromIntegral $ div height 2

toCrapCoordinates :: (Num a) => (a,a) -> (a,a)
toCrapCoordinates (x,y) = (x,(fromIntegral height)-y)

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    void $ go empty screen clockSession_ pos dir_Vec vel-- (V2 0 0)

    where
    go keysDown screen s wpos wdir wvel = do
        keysDown' <- parseEvents keysDown
        (ds, s') <- stepSession s
        --------
        (ex, wp') <- stepWire wpos ds (Right keysDown')
        (ed, wdir') <- stepWire wdir ds (Right keysDown')
        (ev, wvel') <- stepWire wvel ds (Right keysDown')
        let p = either (const 0) id ex
        let (x,y) = toCrapCoordinates (p ^._x, p ^._y)
        let dir = either (const 0) id ed
        let (or_x, or_y) = (f*(dir ^._x), -f*(dir ^._y)) where f = 20.0 :: Double
        let velV = either (const 0) id ev
        let (vel_x, vel_y) = (f*(velV ^._x), -f*(velV ^._y)) where f = 1.0 :: Double
        --putStrLn (show or_x)	
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 100 70 50 >>= SDL.fillRect screen (Just $ SDL.Rect (round xP-75) (round (yP-75)) 150 150)
	(SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x-20) (round (y-20)) 40 40)
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 200 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+or_x)) (round (y-10+or_y)) 20 20)
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 200 0 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-10+vel_x)) (round (y-10+vel_y)) 20 20)
        
        
        SDL.flip screen
        SDL.delay (1000 `div` 60)
        go keysDown' screen s' wp' wdir' wvel'

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

pos :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
pos = integrateVec (V2 100 100) . vel 

vel :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
vel = integrateVec (V2 0 0) . acc

acc :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
acc  = let thrustacc = (*)<$>thrustlevel<*>dir_Vec
	   gravityacc = (/)<$>graV<*>distW
	in (+)<$>thrustacc<*>gravityacc
 
thrustlevel :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
thrustlevel = pure thrust . when (keyDown SDL.SDLK_UP)
--              <|> pure (-thrust). when (keyDown SDL.SDLK_DOWN)
                <|> pure 0

turning_rate :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym)  Double
turning_rate = pure (agility) . when (keyDown SDL.SDLK_LEFT) 
        <|> pure (-agility) . when (keyDown SDL.SDLK_RIGHT) 
        <|> pure 0

orientation :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym)  Double
orientation = integral (0) . turning_rate

dir_Vec :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
dir_Vec = angle <$> orientation 


-- Gravity
graV :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
graV = (-)<$>pos<*>(pure $ V2 xP yP)

distW ::  (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym)  (V2 Double)
distW = pure <$> (fmap quadrance graV)

integrateVec :: (Fractional a, HasTime t s) => (V2 a) -> Wire s e m (V2 a) (V2 a)
integrateVec x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
    in x' `seq` (Right x', integral (x' + dt*dx))

deriving instance Ord SDL.Keysym
