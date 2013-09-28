module Main where

import Control.Lens.Geometry
import Control.Monad
import Data.Default
import Game.Color
import Game.World
import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import System.Random

maxSpeed :: Float
maxSpeed = 8 -- per frame

acceleration :: Float
acceleration = 20 -- not sure exactly

friction :: Float
friction = 5

frameRate :: Int
frameRate = 5

background :: Color
background = black

renderGame :: World -> IO Picture
renderGame w = return . pictures . reverse
             $ drawAccel (w ^. playerPos) (w ^. playerAccel)
             : drawSpeed (w ^. playerPos) (w ^. playerSpeed . cartesian)
             : drawCircle 1 (w ^. playerPos, w ^. playerColor)
             : zipWith drawCircle (map (/30) [26,25..]) (w ^. tails)
    where drawCircle f (m, c) = color (transparent f c)
                                 . uncurry translate (m ^. tuple)
                                 $ shape (30 * f)
          shape = join rectangleSolid
          drawAccel pos vec' = if w ^. debug
              then let vec = vec' & polar.magnitude *~ 1.5
                    in color magenta
                     . uncurry translate (pos ^. tuple)
                     . rotate (negate $ vec ^. polar.angle * 180 / pi - 90)
                     $ polygon [ (-2, 0), (-2, vec ^. polar.magnitude)
                               , (2, vec ^. polar.magnitude), (2, 0) ]
              else blank
          drawSpeed :: Cartesian Float -> Cartesian Float -> Picture
          drawSpeed pos vec' = if w ^. debug
              then let vec = vec' & polar.magnitude *~ 8
                    in color cyan
                     . uncurry translate (pos ^. tuple)
                     . rotate (negate $ vec ^. polar.angle * 180 / pi - 90)
                     $ polygon [ (-2, 0), (-2, vec ^. polar.magnitude)
                               , (2, vec ^. polar.magnitude), (2, 0) ]
              else blank

handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey kd) Down _ _) w
    | kd == KeyDown  = return $ w & playerAccel . y -~ acceleration
    | kd == KeyUp    = return $ w & playerAccel . y +~ acceleration
    | kd == KeyLeft  = return $ w & playerAccel . x -~ acceleration
    | kd == KeyRight = return $ w & playerAccel . x +~ acceleration
handleEvent (EventKey (SpecialKey kd) Up _ _) w
    | kd == KeyDown  = return $ w & playerAccel . y +~ acceleration
    | kd == KeyUp    = return $ w & playerAccel . y -~ acceleration
    | kd == KeyLeft  = return $ w & playerAccel . x +~ acceleration
    | kd == KeyRight = return $ w & playerAccel . x -~ acceleration
handleEvent (EventKey (Char 'a') st _ _) w
    | st == Down = return $ w & playerTurbo .~ True
                              & playerColor .~ green
                              & playerTailColor .~ green
    | otherwise = return $ w & playerTurbo .~ False
                             & playerColor .~ white
                             & playerTailColor .~ white
handleEvent _ w = return w

tickGame :: Float -> World -> IO World
tickGame f w = do
    g <- newStdGen
    let newW = w & playerSpeed <>~ (w ^. playerAccel.polar & magnitude %~ (f *) . min acceleration)
                 & playerSpeed <>~ (w ^. playerSpeed & magnitude *~ f
                                                     & angle %~ invertAngle)
                 & playerSpeed . magnitude %~ min maxSpeed
                 & tails %~ (take 30 . ((jitter w g $ newW ^. playerPos, newW ^. playerTailColor):))
        newNewW = newW & playerPos <>~ ((newW ^. playerSpeed & magnitude *~ speedFactor) ^. cartesian)
    return newNewW
    where
        speedFactor = (* 60) . (* f) $ if w ^. playerTurbo then 2 else 1
        invertAngle x' | x' >= pi = x' - pi
                       | otherwise = x' + pi
        jitter w' g p = let j = w' ^. shadowJitter
                            (xJ, g') = randomR (-j, j) g
                            (yJ, _)  = randomR (-j, j) g'
                         in p & x +~ xJ & y +~ yJ

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
