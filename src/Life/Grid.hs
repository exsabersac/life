-- | 有限矩形网格（死边界 Moore 邻域）。
module Life.Grid
  ( Grid (..)
  , mkGrid
  , emptyGrid
  , gridWidth
  , gridHeight
  , inBounds
  , getCell
  , setCell
  , toggleCell
  , fromAliveCoords
  , aliveCoords
  ) where

import           Data.Array (Array, array, bounds, listArray, (!), (//))
import           Life.Types (Cell (..), Coord, Height, Width)

-- | 单元按下标 @(x, y)@ 存放，满足 @0 <= x < w@、@0 <= y < h@。
--
-- 内部用 @Data.Array@ 的 @Ix (Int, Int)@ 顺序（第二分量变化最快）。
data Grid = Grid
  { gridCells :: !(Array Coord Cell)
  } deriving (Eq, Show)

gridWidth :: Grid -> Width
gridWidth (Grid a) =
  let ((x0, _), (x1, _)) = bounds a
  in x1 - x0 + 1

gridHeight :: Grid -> Height
gridHeight (Grid a) =
  let ((_, y0), (_, y1)) = bounds a
  in y1 - y0 + 1

-- | 从按行展开的列表建网格：外层 y（0..h-1），内层 x（0..w-1）。
-- 内部再转成 @Ix@ 关联顺序。
mkGrid :: Width -> Height -> [Cell] -> Maybe Grid
mkGrid w h cells
  | w <= 0 || h <= 0      = Nothing
  | length cells /= w * h = Nothing
  | otherwise =
      let bnds = ((0, 0), (w - 1, h - 1))
          -- 输入顺序：y 外、x 内（自然行扫描）
          byRow = listArray ((0, 0), (h - 1, w - 1)) cells
          assocs =
            [ ((x, y), byRow ! (y, x))
            | x <- [0 .. w - 1]
            , y <- [0 .. h - 1]
            ]
      in Just $ Grid $ array bnds assocs

-- | 全死网格；尺寸非法时抛错（调用方应保证 w、h > 0）。
emptyGrid :: Width -> Height -> Grid
emptyGrid w h =
  case mkGrid w h (replicate (w * h) Dead) of
    Just g  -> g
    Nothing -> error "emptyGrid: invalid size"

-- | 坐标是否在网格内。
inBounds :: Grid -> Coord -> Bool
inBounds g (x, y) =
  x >= 0 && y >= 0 && x < gridWidth g && y < gridHeight g

-- | 读取单元；越界视为死（死边界）。
getCell :: Grid -> Coord -> Cell
getCell g c
  | inBounds g c = gridCells g ! c
  | otherwise    = Dead  -- 死边界

-- | 写入单元；越界则原样返回。
setCell :: Grid -> Coord -> Cell -> Grid
setCell g c cell
  | inBounds g c = Grid (gridCells g // [(c, cell)])
  | otherwise    = g

-- | 切换存活/死亡；越界无操作。
toggleCell :: Grid -> Coord -> Grid
toggleCell g c =
  case getCell g c of
    Alive -> setCell g c Dead
    Dead  -> setCell g c Alive

-- | 空网格后按坐标列表点亮。
fromAliveCoords :: Width -> Height -> [Coord] -> Grid
fromAliveCoords w h coords =
  foldl (\g c -> setCell g c Alive) (emptyGrid w h) coords

-- | 当前所有存活单元坐标（行优先扫描）。
aliveCoords :: Grid -> [Coord]
aliveCoords g =
  [ (x, y)
  | y <- [0 .. gridHeight g - 1]
  , x <- [0 .. gridWidth g - 1]
  , getCell g (x, y) == Alive
  ]
