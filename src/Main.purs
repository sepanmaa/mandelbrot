module Main where

import Prelude 
import DOM (DOM)
import Signal (Signal, (~>)) 
import Signal as S
import Signal.DOM (mousePos, mouseButton)
import Graphics.Canvas (CANVAS, CanvasElement, ImageData)
import Graphics.Canvas as C 
import Data.Maybe (Maybe(..))
import Data.Int (floor, toNumber)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)

import Mandelbrot.Algorithm (mandelbrot, Rect, zoom, coordinates)
import Mandelbrot.Color (toColors)

foreign import setImageData :: forall e. ImageData -> Array Number -> Eff (canvas :: CANVAS | e) ImageData

foreign import canvasBoundingRect :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Rect


draw :: forall e. CanvasElement -> Rect -> Eff (canvas :: CANVAS | e) Unit
draw canvas rect = do
  ctx <- C.getContext2D canvas
  width <- C.getCanvasWidth canvas
  height <- C.getCanvasHeight canvas
  emptyData <- C.createImageData ctx width height
  values <- setImageData emptyData (toColors (mandelbrot 320 (floor width) (floor height) rect))
  C.putImageData ctx values 0.0 0.0
  pure unit


update :: forall e. CanvasElement -> Eff (dom :: DOM, canvas :: CANVAS | e) (Signal Rect)
update canvas = do
  position <- mousePos
  button <- mouseButton 0
  width <- C.getCanvasWidth canvas
  height <- C.getCanvasHeight canvas
  bounds <- canvasBoundingRect canvas
  let rect = (coordinates width height)
  pure (S.foldp
         (\p r -> let x = ((toNumber p.x) - bounds.x)
                      y = ((toNumber p.y) - bounds.y)
                  in if x > 0.0 && y > 0.0 && x < bounds.w && y < bounds.h
                     then zoom (x/width) (y/height) r
                     else r)
         rect
         (S.sampleOn (S.filter ((==) true) false button) position))
  

main :: forall e. Eff (canvas :: CANVAS, console :: CONSOLE, dom :: DOM | e) Unit
main = do
  c <- C.getCanvasElementById "canvas"
  case c of
    Nothing -> logShow "Canvas not found."
    Just canvas -> do
      zoomSignal <- update canvas
      S.runSignal (zoomSignal ~> draw canvas)
