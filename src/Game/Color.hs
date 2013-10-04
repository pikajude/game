module Game.Color where

import ClassyPrelude
import Control.Lens
import Graphics.Gloss

_red, _green, _blue, _alpha :: Lens' Color Float

_red = _rgba._1
_green = _rgba._2
_blue = _rgba._3
_alpha = _rgba._4

{-# INLINE _red #-}
{-# INLINE _green #-}
{-# INLINE _blue #-}
{-# INLINE _alpha #-}

_rgba :: Lens' Color (Float, Float, Float, Float)
_rgba inj c = (\(r',g',b',a') -> makeColor r' g' b' a') <$> inj (rgbaOfColor c)
{-# INLINE _rgba #-}

traverseRGBA :: Traversal' Color Float
traverseRGBA inj c = let (r', g', b', a') = rgbaOfColor c
                      in makeColor <$> inj r' <*> inj g' <*> inj b' <*> inj a'

transparent :: Float -> Color -> Color
transparent f c = makeColor r g b (a * f) where
      (r,g,b,a) = rgbaOfColor c

-- | Inclusive on both ends.
gradient :: Color -> Color -> Int -> [Color]
gradient c1 c2 r = c1 :
    [ makeColor (r1 + rD * f) (g1 + gD * f) (b1 + bD * f) (a1 + aD * f)
    | x <- [1..r - 1], let f :: Float; f = fromIntegral x / (fromIntegral r - 1) ] where
    (r1, g1, b1, a1) = rgbaOfColor c1
    (r2, g2, b2, a2) = rgbaOfColor c2
    (rD, gD, bD, aD) = ( r2 - r1
                       , g2 - g1
                       , b2 - b1
                       , a2 - a1 )
