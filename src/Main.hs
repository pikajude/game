module Main where

import Control.Lens.Geometry
import Data.Default
import Debug.Trace
import Game.World
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Random

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
renderGame w = return . pictures . reverse
             $ drawCircle (w ^. playerPos) (w ^. playerColor)
             : map (uncurry drawCircle) (w ^. shadows)
    where drawCircle m c = color c $ translate (m ^. x) (m ^. y) $ circleSolid 10

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey kd) Down _ _) w
    | kd == KeyDown  = return $ w & playerAccelDelta <>~ (0        +: negate 1)
    | kd == KeyUp    = return $ w & playerAccelDelta <>~ (0        +:        1)
    | kd == KeyLeft  = return $ w & playerAccelDelta <>~ (negate 1 +:        0)
    | kd == KeyRight = return $ w & playerAccelDelta <>~ (1        +:        0)
handleEvent (EventKey (SpecialKey kd) Up _ _) w
    | kd == KeyDown  =
        let newW = w & playerAccelDelta <>~ (0 +: 1)
         in return $ if w ^. playerAccelDelta.y /= 1
                then newW & playerAccel.cartesian.y .~ 0
                else newW
    | kd == KeyUp    =
        let newW = w & playerAccelDelta <>~ (0 +: negate 1)
         in return $ if w ^. playerAccelDelta.y /= negate 1
                then newW & playerAccel.cartesian.y .~ 0
                else newW
    | kd == KeyLeft  =
        let newW = w & playerAccelDelta <>~ (1 +: 0)
         in return $ if w ^. playerAccelDelta.x /= 1
                then newW & playerAccel.cartesian.x .~ 0
                else newW
    | kd == KeyRight =
        let newW = w & playerAccelDelta <>~ (negate 1 +: 0)
         in return $ if w ^. playerAccelDelta.x /= negate 1
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
                     & shadows %~ (take 30 . ((newW ^. playerPos, newW ^. playerColor):))
        newNewW = newW & playerPos <>~ (newW ^. playerSpeed . cartesian)
    return $ colorize newNewW
    where
        invertAngle x' | x' >= pi = x' - pi
                       | otherwise = x' + pi
        capAccel w' = w' & playerAccel.magnitude %~ min maxAcceleration
        colorize w' = let (c:_, w'') = w' & colorStream <<%~ tail in w'' & playerColor .~ c

main :: IO ()
main = do
    gen <- newStdGen
    playIO (InWindow "main" (400, 400) (400, 200))
           background
           frameRate
           def { _colorStream = randoms gen }
           renderGame
           handleEvent
           tickGame
