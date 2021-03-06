{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Working with vectors.
module Control.Lens.Geometry.Vector (
  -- * Types
  OrderedPair(..),

  Polar,
  Cartesian,

  -- * Vector conversion
  polar,
  cartesian,
  tuple,

  -- * Individual accessors
  magnify,
  invert,

  magnitude,
  angle,
  x,
  y
) where

import ClassyPrelude
import Control.Lens hiding (magnify)
import Data.Default

-- | Represents a cartesian vector @(x, y)@.
data Cartesian a = Cartesian
         { _x :: a
         , _y :: a
         }

instance Default a => Default (Cartesian a) where def = Cartesian def def

makeLenses ''Cartesian

-- | Represents a polar vector @(r, &#x3b8;)@.
data Polar a = Polar
         { _magnitude :: a
         , _angle :: a
         }

instance Default a => Default (Polar a) where def = Polar def def

magnify :: RealFloat a => a -> Polar a -> Polar a
magnify f (Polar r t) = Polar (r * f) t

invert :: RealFloat a => Cartesian a -> Cartesian a
invert (Cartesian a b) = Cartesian (negate a) (negate b)

makeLenses ''Polar

-- | Provides a constructor for ordered-pair-like types.
class OrderedPair p a where
    -- | GHC doesn't allow overloading of @(,)@.
    (+:) :: a -> a -> p a
    decompose :: p a -> (a, a)

infixr 1 +:

instance RealFloat a => OrderedPair Cartesian a where
    (+:) = Cartesian
    decompose (Cartesian x' y') = (x', y')
instance RealFloat a => OrderedPair Polar a where
    (+:) = Polar
    decompose (Polar r t) = (r, t)

instance RealFloat a => Semigroup (Cartesian a) where
    Cartesian x1 y1 <> Cartesian x2 y2 = Cartesian (x1 + x2) (y1 + y2)

instance RealFloat a => Monoid (Cartesian a) where
    mempty = Cartesian 0 0
    mappend = (<>)

instance RealFloat a => Semigroup (Polar a) where
    p1 <> p2 = (view cartesian p1 <> view cartesian p2) ^. polar

instance RealFloat a => Monoid (Polar a) where
    mempty = Polar 0 0
    mappend = (<>)

-- conversion to/from polar
toPolar :: RealFloat a => Cartesian a -> Polar a
toPolar (Cartesian x1 y1) = Polar r d'
     where r = sqrt $ x1 ** 2 + y1 ** 2
           d = atan2 y1 x1
           d' = if d < 0 then 2 * pi + d else d
{-# INLINE toPolar #-}

fromPolar :: RealFloat a => Polar a -> Cartesian a
fromPolar (Polar r d) = Cartesian
                { _x = r * cos d
                , _y = r * sin d }
{-# INLINE fromPolar #-}

-- | >>> (3.0 +: 4.0) ^. polar
-- (5.0,0.9272952180016122)
--
polar :: RealFloat a => Lens' (Cartesian a) (Polar a)
polar f c = fmap fromPolar (f $ toPolar c)
{-# INLINE polar #-}

-- | >>> (2 +: pi / 6) ^. cartesian
-- (1.732050807,1)
cartesian :: RealFloat a => Lens' (Polar a) (Cartesian a)
cartesian f p = fmap toPolar (f $ fromPolar p)
{-# INLINE cartesian #-}

tuple :: (Functor f, OrderedPair p b, OrderedPair p1 a)
      => ((a, a) -> f (b, b)) -> p1 a -> f (p b)
tuple f p = fmap (uncurry (+:)) (f $ decompose p)
