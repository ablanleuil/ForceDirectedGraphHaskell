{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad      (forM_, unless)
import           Data.Graph         as G
import           Data.Maybe
import           Data.Vector        as V
import           Graph
import           Linear             (V2 (..), V4 (..))
import           SDL
import           SDL.Vect
import           SDL.Video.Renderer

main :: IO ()
main = do
  initializeAll

  let mon_graph = (buildG (0,5) [(0, 1), (0, 2), (1, 3)
                                ,(2, 4), (3, 4)
                                ,(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)]
                  , V.fromList [ GNode (Vec2 (100, 100)) (Vec2 (0, 0))
                               , GNode (Vec2 (100, 300)) (Vec2 (0, 0))
                               , GNode (Vec2 (300, 300)) (Vec2 (0, 0))
                               , GNode (Vec2 (500, 300)) (Vec2 (0, 0))
                               , GNode (Vec2 (400, 400)) (Vec2 (0, 0))
                               , GNode (Vec2 (100, 200)) (Vec2 (0, 0))])


  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop (-1) mon_graph renderer

appLoop :: Int -> GraphState -> Renderer -> IO ()
appLoop dragi g@(gr, v) renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = Prelude.any eventIsQPress events

  (P (V2 mx' my')) <- getAbsoluteMouseLocation
  let mx = realToFrac mx'
      my = realToFrac my'
  mb <- getMouseButtons


  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255

  V.forM_ v $ \(GNode (Vec2 (x, y)) _) -> do
    let xi = floor x
        yi = floor y
    fillRect renderer (Just (Rectangle (P $ V2 (xi-25) (yi-25)) (V2 50 50)))

  Control.Monad.forM_ (G.edges gr) $ \(i, j) -> do
    let Vec2 (x1, y1) = pos (v ! i)
        Vec2 (x2, y2) = pos (v ! j)
    drawLine renderer (P $ V2 (floor x1) (floor y1)) (P $ V2 (floor x2) (floor y2))
  present renderer

  let (ngr, nv) = updateGraphState 1 g

  let ndragi
        | not $ mb ButtonLeft = -1
        | dragi /= (-1) = dragi
        | otherwise = fromMaybe (-1) $ V.findIndex (\(GNode (Vec2 (x,y)) _) ->
          mx > x-25 && mx < x+25 && my > y-25 && my < y+25) v

  let nv' = if ndragi /= -1 then
              nv // [(ndragi, GNode (Vec2 (mx, my)) (Vec2 (0,0)))]
            else nv

  unless qPressed (appLoop ndragi (ngr, nv') renderer)
