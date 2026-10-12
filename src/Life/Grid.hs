-- | Finite rectangular grid of cells (dead-border Moore neighbourhood).
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

-- | Cells indexed by @(x, y)@ with @0 <= x < w@, @0 <= y < h@.
--
-- Uses @Data.Array@'s @Ix (Int, Int)@ order (second component varies fastest).
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

-- | Build a grid from a row-wise list: for each y from 0..h-1, x from 0..w-1.
-- Internally converted to @Ix@ association order.
mkGrid :: Width -> Height -> [Cell] -> Maybe Grid
mkGrid w h cells
  | w <= 0 || h <= 0      = Nothing
  | length cells /= w * h = Nothing
  | otherwise =
      let bnds = ((0, 0), (w - 1, h - 1))
          -- input order: y outer, x inner (natural row scan)
          byRow = listArray ((0, 0), (h - 1, w - 1)) cells
          assocs =
            [ ((x, y), byRow ! (y, x))
            | x <- [0 .. w - 1]
            , y <- [0 .. h - 1]
            ]
      in Just $ Grid $ array bnds assocs

emptyGrid :: Width -> Height -> Grid
emptyGrid w h =
  case mkGrid w h (replicate (w * h) Dead) of
    Just g  -> g
    Nothing -> error "emptyGrid: invalid size"

inBounds :: Grid -> Coord -> Bool
inBounds g (x, y) =
  x >= 0 && y >= 0 && x < gridWidth g && y < gridHeight g

getCell :: Grid -> Coord -> Cell
getCell g c
  | inBounds g c = gridCells g ! c
  | otherwise    = Dead  -- dead border

setCell :: Grid -> Coord -> Cell -> Grid
setCell g c cell
  | inBounds g c = Grid (gridCells g // [(c, cell)])
  | otherwise    = g

toggleCell :: Grid -> Coord -> Grid
toggleCell g c =
  case getCell g c of
    Alive -> setCell g c Dead
    Dead  -> setCell g c Alive

fromAliveCoords :: Width -> Height -> [Coord] -> Grid
fromAliveCoords w h coords =
  foldl (\g c -> setCell g c Alive) (emptyGrid w h) coords

aliveCoords :: Grid -> [Coord]
aliveCoords g =
  [ (x, y)
  | y <- [0 .. gridHeight g - 1]
  , x <- [0 .. gridWidth g - 1]
  , getCell g (x, y) == Alive
  ]
