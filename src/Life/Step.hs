-- | One-generation step for a B/S rule on a finite grid (dead border).
module Life.Step
  ( neighborCount
  , step
  , stepN
  ) where

import           Data.Array (array, bounds, range)
import           Life.Grid  (Grid (..), getCell)
import           Life.Rule  (Rule, born, survives)
import           Life.Types (Cell (..), Coord, fromAlive, isAlive)

-- | Eight Moore neighbours; out-of-bounds count as dead (dead border).
neighborCount :: Grid -> Coord -> Int
neighborCount g (x, y) =
  length
    [ ()
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , not (dx == 0 && dy == 0)
    , isAlive (getCell g (x + dx, y + dy))
    ]

stepCell :: Rule -> Grid -> Coord -> Cell
stepCell rule g c =
  let n = neighborCount g c
  in case getCell g c of
       Alive -> fromAlive (survives rule n)
       Dead  -> fromAlive (born rule n)

-- | Advance one generation. New array is built in @Ix@ order via @range@.
step :: Rule -> Grid -> Grid
step rule g =
  let bnds = bounds (gridCells g)
      cells = [(c, stepCell rule g c) | c <- range bnds]
  in Grid (array bnds cells)

stepN :: Int -> Rule -> Grid -> Grid
stepN n rule g
  | n <= 0    = g
  | otherwise = stepN (n - 1) rule (step rule g)
