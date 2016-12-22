module Mandelbrot.Algorithm where

import Prelude
import Data.Array as Array
import Data.Int (toNumber)
import Data.Function.Uncurried (Fn5, runFn5)

foreign import calculatePixel :: Fn5 Number Number Number Number Int Number

type Rect = { x :: Number
            , y :: Number
            , w :: Number
            , h :: Number
            }
            
zoom :: Number -> Number -> Rect -> Rect
zoom px py rect =
  let w' = rect.w / 2.0
      h' = rect.h / 2.0
      x' = (px * rect.w + rect.x) - (w'/2.0)
      y' = (py * rect.h + rect.y) - (h'/2.0)
  in { x: x', y: y', w: w', h: h' }
  

-- initial visible area
coordinates :: Number -> Number -> Rect
coordinates w h =
  let aspect = w / h
      w' = aspect * 2.0
      h' = 2.0
      x' = -w'/2.0 -0.5
      y' = -h'/2.0
  in { x: x', y: y', w: w', h: h' } 

mandelbrot :: Int -> Int -> Int -> Rect -> Array Number
mandelbrot maxIter width height rect = do
  y <- Array.range 0 (height-1)
  x <- Array.range 0 (width-1)
  let xPos = ((toNumber x) / (toNumber width))
      yPos = ((toNumber y) / (toNumber height))
      x' = rect.x + xPos * rect.w
      y' = rect.y + yPos * rect.h
  [(runFn5 calculatePixel 0.0 0.0 x' y' maxIter) / (toNumber maxIter)]
