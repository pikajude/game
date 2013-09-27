{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.World where

import Control.Lens.Geometry
import Data.Default
import Graphics.Gloss
import System.Random

data World = World
           { _playerAccel      :: Cartesian Float
           , _playerFriction   :: Polar Float
           , _playerSpeed      :: Polar Float
           , _playerTurbo      :: Bool
           , _playerPos        :: Cartesian Float
           , _playerColor      :: Color
           , _tails            :: [(Cartesian Float, Color)]
           , _playerTailColor  :: Color
           , _shadowJitter     :: Float
           , _debug            :: Bool
           } deriving Show

makeLenses ''World

instance Default World where
    def = World
        { _playerAccel     = def
        , _playerFriction  = def
        , _playerSpeed     = def
        , _playerTurbo     = False
        , _playerPos       = def
        , _playerColor     = white
        , _playerTailColor = white
        , _tails           = []
        , _shadowJitter    = 4.0
        , _debug           = False
        }

instance Random Color where
    random g = let (redF, g') = random g
                   (greenF, g'') = random g'
                   (blueF, g''') = random g''
                in (makeColor redF greenF blueF 1, g''')

    randomR _ = random
