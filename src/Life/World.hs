-- | 纯仿真世界：输入折叠进 World；时间经 'Life.Stream' 推进。
--
-- SDL 壳只负责：轮询事件 → 映射为 'Input' → 'applyInputs' / 'tick' → 绘制。
-- 存档等 IO 留在边界；加载成功后以 'InSetGrid' 注入。
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

-- | 面向 SDL 的仿真状态（纯）。
data World = World
  { wGrid      :: !Grid
  , wRule      :: !Rule
  , wRuleLabel :: !String
  , wPresetIx  :: !Int
  , wPatternIx :: !Int
  , wPaused    :: !Bool
  , wSpeed     :: !Int -- 运行时每秒世代数（1..30）
  , wAccMs     :: !Int -- 帧时间累加器（毫秒）
  , wCellPx    :: !Int -- 单元格像素边长
  , wQuit      :: !Bool
  } deriving (Eq, Show)

-- | 键盘 / 鼠标意图（存盘 IO 在壳层；加载结果变成 'InSetGrid'）。
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

-- | 初始世界：默认暂停、速度 5 代/秒、单元格 16px。
initialWorld :: Grid -> Rule -> String -> World
initialWorld g rule label =
  World g rule label 0 0 True 5 0 16 False

-- | 按序折叠一组输入。
applyInputs :: [Input] -> World -> World
applyInputs is w = foldl (flip applyInput) w is

-- | 应用单条输入（纯）。
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

-- | 按墙钟 @frameMs@ 推进。未暂停时，经 'splitAtS' 从 'generations'
-- 消费 @n@ 代（不用列表 @iterate@ / @!!@）。
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
