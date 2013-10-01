{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ClassyPrelude hiding (Down)
import Control.Lens.Geometry
import Control.Monad ((<=<))
import Data.Default
import qualified Data.HashMap.Strict as H
import Game.World
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

frameRate :: Int
frameRate = 100

background :: Color
background = black

renderGame :: World -> IO Picture
renderGame w = fmap pictures $ mapM (\(t,e) -> (e ^. render) (t,e) w) (H.toList $ w ^. entities)

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
handleEvent (EventKey (Char 'a') st _ _) w = return $ w & player.metadata.turbo .~ (st == Down)
handleEvent _ w = return w

tickGame :: Float -> World -> IO World
tickGame f w = go (H.toList $ w ^. entities) w
    where
        go (e:es) w' = do
            (k',e') <- tickEntity f e w'
            go es (w' & entities.ix k' .~ e')
        go [] w' = return w'

tickEntity :: Float -> (Text, Entity World) -> World -> IO (Text, Entity World)
tickEntity f (k,entity) _ = do
    p <- applyAll f (entity ^. beforeTick) entity
    let p' = p & behavior.speed <>~ (p ^. behavior.acceleration.polar & magnitude %~ (f *) . min (p ^. behavior.physics.accelerationDelta))
               & behavior.speed <>~ (p ^. behavior.speed & magnitude *~ (p ^. behavior.physics.frictionDelta * f)
                                                         & angle %~ invertAngle)
               & behavior.speed . magnitude %~ min (p ^. behavior.physics.maximumSpeed)
        pPos = p' & behavior.position <>~
                  ((p' ^. behavior.speed & magnitude *~ speedFactor) ^. cartesian)
    b' <- applyAll f (entity ^. afterTick) pPos
    return (k, b')
    where
        invertAngle x' | x' >= pi = x' - pi
                       | otherwise = x' + pi
        speedFactor = 60 * f

applyAll :: (Monad m, MonoFoldable c, Element c ~ (t -> b -> m b))
         => t -> c -> b -> m b
applyAll f = foldr ((<=<) . ($ f)) return

main :: IO ()
main = do
    ar <- getArgs
    let de = "--debug" `elem` ar || "-d" `elem` ar
    playIO (InWindow "game" (400, 400) (200, 100))
           background
           frameRate
           (def & debug .~ de)
           renderGame
           handleEvent
           tickGame
