{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.World (
  Physics,
  accelerationDelta,
  frictionDelta,
  maximumSpeed,

  Behavior,
  acceleration,
  speed,
  position,
  physics,

  Entity,
  behavior,
  metadata,
  name,
  beforeTick,
  afterTick,
  render,

  World,
  tails,
  entities,
  shadowJitter,
  turbo,
  debug,

  player,
) where

import ClassyPrelude
import Control.Lens.Geometry
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

data Metadata w = Player { _tails :: [(Cartesian Float, Color)] }
                | None

makeLenses ''Metadata

data Entity w = Entity
            { _name       :: Text
            , _metadata   :: Metadata (Entity w)
            , _behavior   :: Behavior
            , _beforeTick :: [Float -> Entity w -> IO (Entity w)]
            , _afterTick  :: [Float -> Entity w -> IO (Entity w)]
            , _render     :: (Text, Entity w) -> w -> IO Picture
            }

makeLenses ''Entity

data World = World
           { _entities         :: HashMap Text (Entity World)
           , _turbo            :: Bool
           , _shadowJitter     :: Float
           , _debug            :: Bool
           }

makeLenses ''World

player :: (Applicative f, Indexable Text p)
       => p (Entity World) (f (Entity World)) -> World -> f World
player = entities.ix "player"

instance Default World where
    def = World
        { _entities        = mapFromList [("player", defaultPlayer), ("square", yellowSquare)]
        , _turbo           = False
        , _shadowJitter    = 4.0
        , _debug           = False
        } where
            yellowSquare = Entity
                { _name = "square"
                , _metadata = None
                , _behavior = Behavior
                    { _acceleration = def
                    , _speed = 3 +: pi / 4
                    , _position = def
                    , _physics = Physics
                        { _accelerationDelta = 0
                        , _frictionDelta = 0
                        , _maximumSpeed = 1 / 0
                        }
                    }
                , _beforeTick = []
                , _afterTick = [\f b -> return $ b & behavior.speed <>~ (f +: 5 * pi / 4)]
                , _render = drawSquare
                }
            defaultPlayer = Entity
                { _name     = "Player"
                , _metadata = Player []
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
                , _beforeTick = [addTails]
                , _afterTick = []
                , _render = renderPlayer
                }

drawSquare :: Monad m => (Text, Entity World) -> World -> m Picture
drawSquare (_,p) _ = return $ pictures [yellowSquare, drawSpeed p, drawAccel p]
    where yellowSquare = uncurry translate (p ^. behavior.position.tuple)
                       . color yellow
                       $ rectangleSolid 30 30

addTails :: Float -> Entity World -> IO (Entity World)
addTails _ e = return $ e & metadata.tails %~ take 30 . ((e ^. behavior.position, white):)

renderPlayer :: Monad m => (Text, Entity World) -> World -> m Picture
renderPlayer (_,p) _ = return . pictures . reverse $
        drawAccel p : drawSpeed p
      : drawCircle (p ^. behavior.position) white 30
      : zipWith (uncurry drawCircle) (p ^. metadata.tails) [30,29..]
    where drawCircle m c s = color c
                           . uncurry translate (m ^. tuple)
                           $ shape s
          shape = join rectangleSolid

drawSpeed :: Entity w -> Picture
drawSpeed p = let vec = vec' & polar.magnitude *~ 8
               in color cyan
                . uncurry translate (pos ^. tuple)
                . rotate (negate $ vec ^. polar.angle * 180 / pi - 90)
                $ polygon [ (-2, 0), (-2, vec ^. polar.magnitude)
                          , (2, vec ^. polar.magnitude), (2, 0) ]
    where pos = p ^. behavior.position
          vec' = p ^. behavior.speed.cartesian

drawAccel :: Entity w -> Picture
drawAccel p = let vec = vec' & polar.magnitude *~ 1.5
               in color magenta
                . uncurry translate (pos ^. tuple)
                . rotate (negate $ vec ^. polar.angle * 180 / pi - 90)
                $ polygon [ (-2, 0), (-2, vec ^. polar.magnitude)
                          , (2, vec ^. polar.magnitude), (2, 0) ]
    where pos = p ^. behavior.position
          vec' = p ^. behavior.acceleration

instance Random Color where
    random g = let (redF :: Float, g') = random g
                   (greenF :: Float, g'') = random g'
                   (blueF :: Float, g''') = random g''
                in (makeColor redF greenF blueF 1, g''')

    randomR _ = random
