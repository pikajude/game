module Game.Color where

import Graphics.Gloss

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
