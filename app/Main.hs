{-# LANGUAGE OverloadedStrings #-}
-- | Thin SDL2 shell: poll events → 'Input's → pure fold / 'tick' → draw.
--
-- Simulation time consumes a 'Life.Stream' of generations when unpaused.
--
-- Controls:
--   Space  pause / resume
--   N      single step (pauses)
--   Tab    cycle rule presets (Conway → HighLife → Seeds)
--   +/-    speed up / down
--   P      cycle built-in patterns (centred)
--   G      load glider (near origin)
--   S      save grid to life-save.cells (or $LIFE_SAVE)
--   L      load grid from same path
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
import qualified Data.Text.IO       as TIO
import           Foreign.C.Types    (CInt)
import           SDL
import           System.Environment (lookupEnv)
import           System.IO          (hPutStrLn, stderr)

import           Life.Cells
import           Life.Grid
import           Life.Pattern       (glider)
import           Life.Rule          (conway)
import           Life.World

defaultW, defaultH, defaultCell, frameMs :: Int
defaultW = 40
defaultH = 30
defaultCell = 16
frameMs = 16

defaultSavePath :: FilePath
defaultSavePath = "life-save.cells"

main :: IO ()
main = do
  seedGlider <- (== Just "1") <$> lookupEnv "LIFE_GLIDER"
  let rule0 = conway
      label0 = "B3/S23"
      grid0 =
        if seedGlider
          then glider defaultW defaultH
          else emptyGrid defaultW defaultH
      world0 = (initialWorld grid0 rule0 label0) { wCellPx = defaultCell }

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
  mapM_ (dispatchEvent ref) events
  w <- readIORef ref
  if wQuit w
    then pure ()
    else do
      let w' = tick frameMs w
      writeIORef ref w'
      when (wRuleLabel w' /= wRuleLabel w) $
        windowTitle window $= T.pack ("life — " ++ wRuleLabel w')
      draw renderer w'
      t1 <- ticks
      let elapsed = fromIntegral (t1 - t0) :: Int
          sleepMs = max 0 (frameMs - elapsed)
      threadDelay (sleepMs * 1000)
      loop renderer window ref

-- | Map one SDL event to pure inputs (save/load IO at the edge).
dispatchEvent :: IORef World -> Event -> IO ()
dispatchEvent ref ev =
  case eventPayload ev of
    QuitEvent ->
      modifyIORef' ref (applyInput InQuit)
    KeyboardEvent ke
      | keyboardEventKeyMotion ke == Pressed
      , not (keyboardEventRepeat ke) ->
          case keysymKeycode (keyboardEventKeysym ke) of
            KeycodeEscape -> modifyIORef' ref (applyInput InQuit)
            KeycodeQ      -> modifyIORef' ref (applyInput InQuit)
            KeycodeSpace  -> modifyIORef' ref (applyInput InTogglePause)
            KeycodeN      -> modifyIORef' ref (applyInput InStepOnce)
            KeycodeR      -> modifyIORef' ref (applyInput InClear)
            KeycodeG      -> modifyIORef' ref (applyInput InLoadGlider)
            KeycodeP      -> modifyIORef' ref (applyInput InCyclePattern)
            KeycodeTab    -> modifyIORef' ref (applyInput InCyclePreset)
            KeycodeEquals -> modifyIORef' ref (applyInput InSpeedUp)
            KeycodePlus   -> modifyIORef' ref (applyInput InSpeedUp)
            KeycodeMinus  -> modifyIORef' ref (applyInput InSpeedDown)
            KeycodeS      -> do
              path <- savePath
              w <- readIORef ref
              TIO.writeFile path (encodeCells (wGrid w))
              hPutStrLn stderr ("saved " ++ path)
            KeycodeL      -> do
              path <- savePath
              txt <- TIO.readFile path
              case decodeCells txt of
                Left err -> hPutStrLn stderr ("load failed: " ++ err)
                Right g  -> do
                  modifyIORef' ref (applyInput (InSetGrid g))
                  hPutStrLn stderr ("loaded " ++ path)
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
            in applyInput (InToggleAt (x, y)) w
      | otherwise -> pure ()
    _ -> pure ()

savePath :: IO FilePath
savePath = do
  m <- lookupEnv "LIFE_SAVE"
  pure $ maybe defaultSavePath id m

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
  mapM_
    (\x ->
       drawLine
         renderer
         (P (V2 (fromIntegral (x * px)) 0))
         (P (V2 (fromIntegral (x * px)) hpx)))
    [0 .. ww]
  mapM_
    (\y ->
       drawLine
         renderer
         (P (V2 0 (fromIntegral (y * px))))
         (P (V2 wpx (fromIntegral (y * px)))))
    [0 .. hh]
  present renderer

fillCell :: Renderer -> Int -> (Int, Int) -> IO ()
fillCell r px (x, y) =
  fillRect
    r
    (Just
       (Rectangle
          (P (V2 (fromIntegral (x * px)) (fromIntegral (y * px))))
          (V2 (fromIntegral px) (fromIntegral px))))
