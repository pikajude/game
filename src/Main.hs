{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens.Geometry
import Data.Default
import Data.Monoid
import Game.World
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

maxSpeed :: Float
maxSpeed = 5 -- x/60 per frame, x per second

acceleration :: Float
acceleration = 8 -- not sure exactly

maxAcceleration :: Float
maxAcceleration = 16 -- pixels per second squared

friction :: Float
friction = 40

frameRate :: Int
frameRate = 60

background :: Color
background = black

renderGame :: World -> IO Picture
renderGame w = return $ color red $ translate x' y' $ circleSolid 10
    where x' = w ^. playerPos . x
          y' = w ^. playerPos . y

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey kd) Down _ _) w
    | kd == KeyDown  = return $ w & playerAccelDelta <>~ (0        +: negate 1)
    | kd == KeyUp    = return $ w & playerAccelDelta <>~ (0        +:        1)
    | kd == KeyLeft  = return $ w & playerAccelDelta <>~ (negate 1 +:        0)
    | kd == KeyRight = return $ w & playerAccelDelta <>~ (1        +:        0)
handleEvent (EventKey (SpecialKey kd) Up _ _) w
    | kd == KeyDown  =
        let newW = w & playerAccelDelta <>~ (0 +: 1)
         in return $ if w ^. playerAccelDelta.y == negate 1
                then newW & playerAccel.cartesian.y .~ 0
                else newW
    | kd == KeyUp    =
        let newW = w & playerAccelDelta <>~ (0 +: negate 1)
         in return $ if w ^. playerAccelDelta.y == 1
                then newW & playerAccel.cartesian.y .~ 0
                else newW
    | kd == KeyLeft  =
        let newW = w & playerAccelDelta <>~ (1 +: 0)
         in return $ if w ^. playerAccelDelta.x == negate 1
                then newW & playerAccel.cartesian.x .~ 0
                else newW
    | kd == KeyRight =
        let newW = w & playerAccelDelta <>~ (negate 1 +: 0)
         in return $ if w ^. playerAccelDelta.x == 1
                then newW & playerAccel.cartesian.x .~ 0
                else newW
handleEvent _ w = return w

tickGame :: Float -> World -> IO World
tickGame f w = do
    let newW'' = capAccel $
           w & playerAccel <>~ magnify (f * acceleration) (w ^. playerAccelDelta.polar)
        newW' = newW'' & playerFriction .~ ( newW'' ^. playerSpeed
                                           & magnitude %~ min friction
                                           & angle %~ invertAngle )
        newW = newW' & playerSpeed <>~ (newW' ^. playerAccel & magnitude *~ f)
                     & playerSpeed <>~ (newW' ^. playerFriction & magnitude *~ f)
        newNewW = newW & playerPos <>~ (newW ^. playerSpeed . cartesian)
    print newNewW
    return newNewW
    where
        invertAngle x' | x' >= pi = x' - pi
                       | otherwise = x' + pi
        capAccel w' = w' & playerAccel.magnitude %~ min maxAcceleration

closeToZero :: Float -> Bool
closeToZero m = abs m <= 1e-6

toZero :: (Ord a, Num a) => a -> a -> a
toZero m n = if m - n < 0 then 0 else m - n

constrainRange :: (Ord a, Num a) => (a,a) -> a -> a
constrainRange (a,b) n
    | n < a = a
    | n > b = b
    | otherwise = n

main :: IO ()
main = -- do
    -- gen <- newStdGen
    playIO (InWindow "main" (400, 400) (400, 200))
           background
           frameRate
           def
           renderGame
           handleEvent
           tickGame
