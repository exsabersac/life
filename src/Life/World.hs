-- | Pure simulation world: inputs fold into World; time advances via 'Life.Stream'.
module Life.World
  ( World (..)
  , Input (..)
  , initialWorld
  , applyInput
  , applyInputs
  , tick
  ) where

import           Life.Grid    (Grid, emptyGrid, gridHeight, gridWidth, toggleCell)
import           Life.Pattern (catalog, glider, stampCentered)
import           Life.Rule    (Rule, presets)
import           Life.Step    (generations)
import           Life.Stream  (headS, splitAtS)
import           Life.Types   (Coord)

-- | SDL-facing simulation state (pure).
data World = World
  { wGrid      :: !Grid
  , wRule      :: !Rule
  , wRuleLabel :: !String
  , wPresetIx  :: !Int
  , wPatternIx :: !Int
  , wPaused    :: !Bool
  , wSpeed     :: !Int -- generations per second when running (1..30)
  , wAccMs     :: !Int
  , wCellPx    :: !Int
  , wQuit      :: !Bool
  } deriving (Eq, Show)

-- | Keyboard / mouse intents (save IO stays at the edge; load becomes 'InSetGrid').
data Input
  = InQuit
  | InTogglePause
  | InStepOnce
  | InClear
  | InLoadGlider
  | InCyclePattern
  | InCyclePreset
  | InSpeedUp
  | InSpeedDown
  | InToggleAt Coord
  | InSetGrid Grid
  deriving (Eq, Show)

initialWorld :: Grid -> Rule -> String -> World
initialWorld g rule label =
  World g rule label 0 0 True 5 0 16 False

applyInputs :: [Input] -> World -> World
applyInputs is w = foldl (flip applyInput) w is

applyInput :: Input -> World -> World
applyInput InQuit w =
  w { wQuit = True }
applyInput InTogglePause w =
  w { wPaused = not (wPaused w) }
applyInput InStepOnce w =
  let g' = headS (snd (splitAtS 1 (generations (wRule w) (wGrid w))))
  in w { wGrid = g', wPaused = True }
applyInput InClear w =
  w { wGrid = emptyGrid (gridWidth (wGrid w)) (gridHeight (wGrid w)) }
applyInput InLoadGlider w =
  w
    { wGrid = glider (gridWidth (wGrid w)) (gridHeight (wGrid w))
    , wPaused = True
    }
applyInput InCyclePattern w = cyclePattern w
applyInput InCyclePreset w = cyclePreset w
applyInput InSpeedUp w =
  w { wSpeed = min 30 (wSpeed w + 1) }
applyInput InSpeedDown w =
  w { wSpeed = max 1 (wSpeed w - 1) }
applyInput (InToggleAt c) w =
  w { wGrid = toggleCell (wGrid w) c, wPaused = True }
applyInput (InSetGrid g) w =
  w { wGrid = g, wPaused = True }

cyclePreset :: World -> World
cyclePreset w =
  let ix = (wPresetIx w + 1) `mod` length presets
      (label, rule) = presets !! ix
  in w { wPresetIx = ix, wRule = rule, wRuleLabel = label, wPaused = True }

cyclePattern :: World -> World
cyclePattern w =
  let n = length catalog
      ix = wPatternIx w `mod` n
      p = catalog !! ix
      g = stampCentered p (gridWidth (wGrid w)) (gridHeight (wGrid w))
  in w { wPatternIx = (ix + 1) `mod` n, wGrid = g, wPaused = True }

-- | Advance by @frameMs@ of wall-clock time. When unpaused, consume @n@
-- generations from 'generations' via 'splitAtS' (not list @iterate@ / @!!@).
tick :: Int -> World -> World
tick frameMs w
  | wPaused w = w
  | otherwise =
      let period = max 1 (1000 `div` max 1 (wSpeed w))
          acc = wAccMs w + frameMs
          (gens, acc') = acc `divMod` period
          g' =
            if gens <= 0
              then wGrid w
              else headS (snd (splitAtS gens (generations (wRule w) (wGrid w))))
      in w { wGrid = g', wAccMs = acc' }
