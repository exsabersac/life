{-# LANGUAGE OverloadedStrings #-}
-- | SDL2 shell: draw a CA grid, pause/step, speed, mouse edit, cycle B/S presets.
--
-- Controls:
--   Space  pause / resume
--   N      single step (pauses)
--   Tab    cycle rule presets (Conway → HighLife → Seeds)
--   +/-    speed up / down
--   G      load glider
--   R      clear grid
--   click  toggle cell (pauses)
--   Esc/Q  quit
module Main
  ( main
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (when)
import           Data.IORef
import qualified Data.Text          as T
import           Foreign.C.Types    (CInt)
import           SDL
import           System.Environment (lookupEnv)

import           Life.Grid
import           Life.Rule
import           Life.Step          (step)

data World = World
  { wGrid      :: !Grid
  , wRule      :: !Rule
  , wRuleLabel :: !String
  , wPresetIx  :: !Int
  , wPaused    :: !Bool
  , wSpeed     :: !Int  -- generations per second when running (1..30)
  , wAccMs     :: !Int
  , wCellPx    :: !Int
  , wQuit      :: !Bool
  }

defaultW, defaultH, defaultCell :: Int
defaultW = 40
defaultH = 30
defaultCell = 16

main :: IO ()
main = do
  seedGlider <- (== Just "1") <$> lookupEnv "LIFE_GLIDER"
  let rule0 = conway
      label0 = "B3/S23"
      grid0 =
        if seedGlider
          then glider defaultW defaultH
          else emptyGrid defaultW defaultH
      world0 = World grid0 rule0 label0 0 True 5 0 defaultCell False

  initializeAll
  let winW = fromIntegral (defaultW * defaultCell) :: CInt
      winH = fromIntegral (defaultH * defaultCell) :: CInt
  window <- createWindow (T.pack ("life — " ++ label0))
    defaultWindow { windowInitialSize = V2 winW winH }
  renderer <- createRenderer window (-1) defaultRenderer
  ref <- newIORef world0
  loop renderer window ref
  destroyRenderer renderer
  destroyWindow window
  quit

loop :: Renderer -> Window -> IORef World -> IO ()
loop renderer window ref = do
  t0 <- ticks
  events <- pollEvents
  mapM_ (handleEvent ref) events
  w <- readIORef ref
  if wQuit w
    then pure ()
    else do
      let w' = advance w
      writeIORef ref w'
      when (wRuleLabel w' /= wRuleLabel w) $
        windowTitle window $= T.pack ("life — " ++ wRuleLabel w')
      draw renderer w'
      t1 <- ticks
      let elapsed = fromIntegral (t1 - t0) :: Int
          sleepMs = max 0 (16 - elapsed)
      threadDelay (sleepMs * 1000)
      loop renderer window ref

advance :: World -> World
advance w
  | wPaused w = w
  | otherwise =
      let period = max 1 (1000 `div` max 1 (wSpeed w))
          acc = wAccMs w + 16
          (gens, acc') = acc `divMod` period
          g' = iterate (step (wRule w)) (wGrid w) !! gens
      in w { wGrid = g', wAccMs = acc' }

handleEvent :: IORef World -> Event -> IO ()
handleEvent ref ev =
  case eventPayload ev of
    QuitEvent -> modifyIORef' ref (\w -> w { wQuit = True })
    KeyboardEvent ke
      | keyboardEventKeyMotion ke == Pressed
      , not (keyboardEventRepeat ke) ->
          case keysymKeycode (keyboardEventKeysym ke) of
            KeycodeEscape -> modifyIORef' ref (\w -> w { wQuit = True })
            KeycodeQ      -> modifyIORef' ref (\w -> w { wQuit = True })
            KeycodeSpace  -> modifyIORef' ref (\w -> w { wPaused = not (wPaused w) })
            KeycodeN      -> modifyIORef' ref $ \w ->
              w { wGrid = step (wRule w) (wGrid w), wPaused = True }
            KeycodeR      -> modifyIORef' ref $ \w ->
              w { wGrid = emptyGrid (gridWidth (wGrid w)) (gridHeight (wGrid w)) }
            KeycodeG      -> modifyIORef' ref $ \w ->
              w { wGrid = glider (gridWidth (wGrid w)) (gridHeight (wGrid w)), wPaused = True }
            KeycodeTab    -> modifyIORef' ref cyclePreset
            KeycodeEquals -> modifyIORef' ref $ \w ->
              w { wSpeed = min 30 (wSpeed w + 1) }
            KeycodePlus   -> modifyIORef' ref $ \w ->
              w { wSpeed = min 30 (wSpeed w + 1) }
            KeycodeMinus  -> modifyIORef' ref $ \w ->
              w { wSpeed = max 1 (wSpeed w - 1) }
            _ -> pure ()
      | otherwise -> pure ()
    MouseButtonEvent me
      | mouseButtonEventMotion me == Pressed
      , mouseButtonEventButton me == ButtonLeft -> do
          let P (V2 mx my) = mouseButtonEventPos me
          modifyIORef' ref $ \w ->
            let px = wCellPx w
                x = fromIntegral mx `div` px
                y = fromIntegral my `div` px
            in w { wGrid = toggleCell (wGrid w) (x, y), wPaused = True }
      | otherwise -> pure ()
    _ -> pure ()

cyclePreset :: World -> World
cyclePreset w =
  let ix = (wPresetIx w + 1) `mod` length presets
      (label, rule) = presets !! ix
  in w { wPresetIx = ix, wRule = rule, wRuleLabel = label, wPaused = True }

draw :: Renderer -> World -> IO ()
draw renderer w = do
  rendererDrawColor renderer $= V4 20 20 28 255
  clear renderer
  let g = wGrid w
      px = wCellPx w
      ww = gridWidth g
      hh = gridHeight g
  rendererDrawColor renderer $= V4 36 36 48 255
  mapM_ (fillCell renderer px) [(x, y) | y <- [0 .. hh - 1], x <- [0 .. ww - 1]]
  rendererDrawColor renderer $= V4 120 220 140 255
  mapM_ (fillCell renderer px) (aliveCoords g)
  rendererDrawColor renderer $= V4 50 50 64 255
  let wpx = fromIntegral (ww * px) :: CInt
      hpx = fromIntegral (hh * px) :: CInt
  mapM_ (\x -> drawLine renderer (P (V2 (fromIntegral (x * px)) 0)) (P (V2 (fromIntegral (x * px)) hpx))) [0 .. ww]
  mapM_ (\y -> drawLine renderer (P (V2 0 (fromIntegral (y * px)))) (P (V2 wpx (fromIntegral (y * px))))) [0 .. hh]
  present renderer

fillCell :: Renderer -> Int -> (Int, Int) -> IO ()
fillCell r px (x, y) =
  fillRect r
    (Just
      (Rectangle
        (P (V2 (fromIntegral (x * px)) (fromIntegral (y * px))))
        (V2 (fromIntegral px) (fromIntegral px))))
