{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens.Geometry
import Control.Monad
import Data.Default
import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Game.World
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Environment

frameRate :: Int
frameRate = 5

background :: Color
background = black

renderGame :: World -> IO Picture
renderGame w = fmap pictures $ mapM (renderPlayer ?? w) (H.toList $ w ^. entities)

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey kd) Down _ _) w
    | kd == KeyDown  = return $ w & player.behavior.acceleration.y -~ (w ^?! player.behavior.physics.accelerationDelta)
    | kd == KeyUp    = return $ w & player.behavior.acceleration.y +~ (w ^?! player.behavior.physics.accelerationDelta)
    | kd == KeyLeft  = return $ w & player.behavior.acceleration.x -~ (w ^?! player.behavior.physics.accelerationDelta)
    | kd == KeyRight = return $ w & player.behavior.acceleration.x +~ (w ^?! player.behavior.physics.accelerationDelta)
handleEvent (EventKey (SpecialKey kd) Up _ _) w
    | kd == KeyDown  = return $ w & player.behavior.acceleration.y +~ (w ^?! player.behavior.physics.accelerationDelta)
    | kd == KeyUp    = return $ w & player.behavior.acceleration.y -~ (w ^?! player.behavior.physics.accelerationDelta)
    | kd == KeyLeft  = return $ w & player.behavior.acceleration.x +~ (w ^?! player.behavior.physics.accelerationDelta)
    | kd == KeyRight = return $ w & player.behavior.acceleration.x -~ (w ^?! player.behavior.physics.accelerationDelta)
handleEvent (EventKey (Char 'a') st _ _) w
    | st == Down = return $ w & turbo .~ True
    | otherwise = return $ w & turbo .~ False
handleEvent _ w = return w

tickGame :: Float -> World -> IO World
tickGame f w = go (H.toList $ w ^. entities) w
    where
        go (e:es) w' = do
            (k',e') <- tickEntity f e w'
            go es (w' & entities.ix k' .~ e')
        go [] w' = return w'

tickEntity :: Float -> (Text, Entity World) -> World -> IO (Text, Entity World)
tickEntity f (k,p) _ = do
    let p' = p & behavior.speed <>~ (p ^. behavior.acceleration.polar & magnitude %~ (f *) . min (p ^. behavior.physics.accelerationDelta))
               & behavior.speed <>~ (p ^. behavior.speed & magnitude *~ (p ^. behavior.physics.frictionDelta * f)
                                                         & angle %~ invertAngle)
               & behavior.speed . magnitude %~ min (p ^. behavior.physics.maximumSpeed)
        pPos = p' & behavior.position <>~ (p' ^. behavior.speed.cartesian)
    b' <- applyAll (pPos ^. transforms) (pPos ^. behavior)
    return (k, pPos & behavior .~ b')
    where
        invertAngle x' | x' >= pi = x' - pi
                       | otherwise = x' + pi

applyAll :: Monad m => [a -> m a] -> a -> m a
applyAll = foldr (<=<) return

main :: IO ()
main = do
    (fr:ar) <- getArgs
    let de = "--debug" `elem` ar || "-d" `elem` ar
    playIO (InWindow "game" (400, 400) (200, 100))
           background
           (read fr)
           def { _debug = de }
           renderGame
           handleEvent
           tickGame
