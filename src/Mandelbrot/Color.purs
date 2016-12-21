module Mandelbrot.Color where

import Prelude
import Math (round)

-- expects hsl values scaled to [0.0-1.0]
hslToRGBA :: Number -> Number -> Number -> Array Number
hslToRGBA h s l =
  let fromHue p q t | t < 0.0 = fromHue p q (t+1.0)
                    | t > 1.0 = fromHue p q (t-1.0)
                    | t < (1.0/6.0) = p + (q - p) * 6.0 * t
                    | t < (1.0/2.0) = q
                    | t < (2.0/3.0) = p + (q - p) * ((2.0/3.0) - t) * 6.0
                    | otherwise = p
      q' = if l < 0.5 then l * (1.0 + s) else l + s - (l * s)
      p' = (2.0 * l) - q'
      r = fromHue p' q' (h + (1.0/3.0))
      g = fromHue p' q' h
      b = fromHue p' q' (h - (1.0/3.0))
  in [round (r*255.0), round (g*255.0), round (b*255.0), 255.0]


toColors :: Array Number -> Array Number
toColors xs = do
  x <- xs
  if x == 0.0
    then [0.0, 0.0, 0.0, 255.0]
    else hslToRGBA (x*0.7) 1.0 (0.5-(x*0.4))
