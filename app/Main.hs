{-# LANGUAGE StandaloneDeriving #-}
import Prelude hiding ((.), id, null, filter)
import Control.Monad (void)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import Linear
import qualified Graphics.UI.SDL as SDL

width = 1280 
height = 720
acc = 40

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    void $ go empty screen clockSession_ pos_x pos_y 0 0

    where
    go keysDown screen s wx wy x y = do
        keysDown' <- parseEvents keysDown
        (ds, s') <- stepSession s
        (ex, wx') <- stepWire wx ds (Right keysDown')
        (ey, wy') <- stepWire wy ds (Right keysDown')
        let x' = either (const 0) id ex
        let y' = either (const 0) id ey

        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 10 10 10 >>= SDL.fillRect screen Nothing

        (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>= SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 50 50)

        SDL.flip screen
        SDL.delay (1000 `div` 60)
        go keysDown' screen s' wx' wy' x' y'

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


vel_x :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (Double)
vel_x = integral 0 . acc_x

pos_x :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (Double)
pos_x = integral ((fromIntegral width)*0.5) . vel_x

acc_x :: (Monad m, Monoid e) => Wire s e m (Set SDL.Keysym) (Double)
acc_x  =  pure (-acc) . when (keyDown SDL.SDLK_LEFT)
             <|> pure acc . when (keyDown SDL.SDLK_RIGHT)
             <|> pure 0

vel_y :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (Double)
vel_y = integral 0 . acc_y

pos_y :: (Monad m, HasTime t s) => Wire s () m (Set SDL.Keysym) (Double)
pos_y = integral ((fromIntegral width)*0.5) . vel_y

acc_y :: (Monad m, Monoid e) => Wire s e m (Set SDL.Keysym) (Double)
acc_y  =  pure (-acc) . when (keyDown SDL.SDLK_UP)
             <|> pure acc . when (keyDown SDL.SDLK_DOWN)
             <|> pure 0

deriving instance Ord SDL.Keysym
