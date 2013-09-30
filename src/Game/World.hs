{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.World where

import ClassyPrelude
import Control.Lens.Geometry
import Data.List.NonEmpty hiding (reverse)
import Data.Default
import Graphics.Gloss
import System.Random

data Physics = Physics
             { _accelerationDelta :: Float
             , _frictionDelta     :: Float
             , _maximumSpeed      :: Float
             } deriving Show

makeLenses ''Physics

data Behavior = Behavior
              { _acceleration :: Cartesian Float
              , _speed        :: Polar Float
              , _position     :: Cartesian Float
              , _physics      :: Physics
              }

makeLenses ''Behavior

instance Show (Behavior -> IO Behavior) where show _ = "[behavior transforms]"

instance Show (a -> b -> IO Picture) where show _ = "render function"

data Entity w = Entity
            { _name       :: Text
            , _colorE     :: Color
            , _behavior   :: Behavior
            , _transforms :: [Behavior -> IO Behavior]
            , _render     :: (Text, Entity w) -> w -> IO Picture
            }

makeLenses ''Entity

data World = World
           { _entities         :: HashMap Text (Entity World)
           , _turbo            :: Bool
           , _tails            :: [(Cartesian Float, Color)]
           , _shadowJitter     :: Float
           , _debug            :: Bool
           }

makeLenses ''World

neHead :: Lens' (NonEmpty a) a
neHead = lens Data.List.NonEmpty.head (\(_ :| ms) m -> m :| ms)

player :: (Applicative f, Indexable Text p)
       => p (Entity World) (f (Entity World)) -> World -> f World
player = entities.ix "player"

instance Default World where
    def = World
        { _entities        = singletonMap "player" defaultPlayer
        , _turbo           = False
        , _tails           = []
        , _shadowJitter    = 4.0
        , _debug           = False
        } where
            defaultPlayer = Entity
                { _name     = "Player"
                , _colorE   = white
                , _behavior = Behavior
                    { _acceleration = def
                    , _speed        = def
                    , _position     = def
                    , _physics      = Physics
                        { _accelerationDelta = 20
                        , _frictionDelta     = 1
                        , _maximumSpeed      = 8
                        }
                    }
                , _transforms = []
                , _render = renderPlayer
                }

renderPlayer :: Monad m => (Text, Entity World) -> World -> m Picture
renderPlayer (_,p) w = return . pictures . reverse $
        drawAccel (p ^. behavior.position) (p ^. behavior.acceleration)
      : drawSpeed (p ^. behavior.position) (p ^. behavior.speed.cartesian)
      : [drawCircle (p ^. behavior.position, p ^. colorE)]
    where drawCircle (m, c) = color c
                            . uncurry translate (m ^. tuple)
                            $ shape 30
          shape = join rectangleSolid
          drawAccel pos vec' = if w ^. debug
              then let vec = vec' & polar.magnitude *~ 1.5
                    in color magenta
                     . uncurry translate (pos ^. tuple)
                     . rotate (negate $ vec ^. polar.angle * 180 / pi - 90)
                     $ polygon [ (-2, 0), (-2, vec ^. polar.magnitude)
                               , (2, vec ^. polar.magnitude), (2, 0) ]
              else blank
          drawSpeed pos vec' = if w ^. debug
              then let vec = vec' & polar.magnitude *~ 8
                    in color cyan
                     . uncurry translate (pos ^. tuple)
                     . rotate (negate $ vec ^. polar.angle * 180 / pi - 90)
                     $ polygon [ (-2, 0), (-2, vec ^. polar.magnitude)
                               , (2, vec ^. polar.magnitude), (2, 0) ]
              else blank

instance Random Color where
    random g = let (redF :: Float, g') = random g
                   (greenF :: Float, g'') = random g'
                   (blueF :: Float, g''') = random g''
                in (makeColor redF greenF blueF 1, g''')

    randomR _ = random
