{-# LANGUAGE TemplateHaskell #-}

module Game.World where

import Control.Lens.Geometry
import Data.Default

data World = World
           { _playerAccelDelta :: Cartesian Float
           , _playerAccel      :: Polar Float
           , _playerFriction   :: Polar Float
           , _playerSpeed      :: Polar Float
           , _playerPos        :: Cartesian Float
           } deriving Show

makeLenses ''World

instance Default World where def = World def def def def def
