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
thrust = 40

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    void $ go empty screen clockSession_ pos (V2 0 0)

    where
    go keysDown screen s wp p = do
        keysDown' <- parseEvents keysDown
        (ds, s') <- stepSession s
        (ex, wp') <- stepWire wp ds (Right keysDown')
        --(ey, wy') <- stepWire wy ds (Right keysDown')
        let p' = either (const 0) id ex
        --let y' = either (const 0) id ey
        let x = p ^._x
            y = p ^._y

        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing

        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 50 50)

        SDL.flip screen
        SDL.delay (1000 `div` 60)
        go keysDown' screen s' wp' p'

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

acc :: (Monad m, Monoid e) => Wire s e m (Set SDL.Keysym) (V2 Double)
acc  =  pure (V2 (-thrust) 0) . when (keyDown SDL.SDLK_LEFT)
             <|> pure (V2 thrust 0) . when (keyDown SDL.SDLK_RIGHT)
             <|> pure (V2 0 0)

integrateVec :: (Fractional a, HasTime t s) => (V2 a) -> Wire s e m (V2 a) (V2 a)
integrateVec x' =
    mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
    in x' `seq` (Right x', integral (x' + dt*dx))

deriving instance Ord SDL.Keysym
