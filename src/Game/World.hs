{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.World where

import Control.Lens.Geometry
import Data.Default
import Graphics.Gloss
import System.Random

data World = World
           { _playerAccelDelta :: Cartesian Float
           , _playerAccel      :: Polar Float
           , _playerFriction   :: Polar Float
           , _playerSpeed      :: Polar Float
           , _playerPos        :: Cartesian Float
           , _playerColor      :: Color
           , _colorStream      :: [Color]
           , _shadows          :: [(Cartesian Float, Color)]
           , _colorTimer       :: Float
           } deriving Show

makeLenses ''World

instance Default World where def = World def def def def def red [] [] 0

instance Random Color where
    random g = let (redF, g') = random g
                   (greenF, g'') = random g'
                   (blueF, g''') = random g''
                in (makeColor redF greenF blueF 1, g''')

    randomR _ = random
