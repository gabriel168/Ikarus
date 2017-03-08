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

width = 1280 
height = 720
thrust = 40.0
agility = 2

toCrapCoordinates :: (Num a) => (a,a) -> (a,a)
toCrapCoordinates (x,y) = (x,(fromIntegral height)-y)
--00 -> 01
--10 -> 11
--01 -> 00
--11 -> 10

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    void $ go empty screen clockSession_ pos dir_Vec -- (V2 0 0)

    where
    go keysDown screen s wpos wdir = do
        keysDown' <- parseEvents keysDown
        (ds, s') <- stepSession s
        (ex, wp') <- stepWire wpos ds (Right keysDown')
	(ed, wdir') <- stepWire wdir ds (Right keysDown')
        let p = either (const 0) id ex
        let (x,y) = toCrapCoordinates (p ^._x, p ^._y)
	let dir = either (const 0) id ed
	let (or_x, or_y) = toCrapCoordinates (f*(dir ^._x), f*(dir ^._y)) where f = 1.0 :: Double
	--putStrLn (show or_x)	
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing
        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x-25) (round (y-25)) 50 50)
	(SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 200 0 >>= SDL.fillRect screen (Just $ SDL.Rect (round (x-15+or_x)) (round (y-25+or_y)) 30 30)

        SDL.flip screen
        SDL.delay (1000 `div` 60)
        go keysDown' screen s' wp' wdir'

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
pos = integrateVec (V2 0 0) . vel

vel :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
vel = integrateVec (V2 0 0) . acc

acc :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
acc  = fmap (*thrust) dir_Vec . when (keyDown SDL.SDLK_UP)
             <|> pure (V2 0 0)

turning_rate :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym)  Double
turning_rate = pure (agility) . when (keyDown SDL.SDLK_LEFT) 
		<|> pure (-agility) . when (keyDown SDL.SDLK_RIGHT) 
		<|> pure 0

orientation :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym)  Double
orientation = integral (0) . turning_rate

dir_Vec :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (V2 Double)
dir_Vec =  angle <$> orientation 

integrateVec :: (Fractional a, HasTime t s) => (V2 a) -> Wire s e m (V2 a) (V2 a)
integrateVec x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
    in x' `seq` (Right x', integral (x' + dt*dx))

deriving instance Ord SDL.Keysym
