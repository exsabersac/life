-- | One-generation step for a B/S rule on a finite grid (dead border),
-- and an infinite generation 'Stream'.
module Life.Step
  ( neighborCount
  , step
  , stepN
  , generations
  ) where

import           Data.Array  (array, bounds, range)
import           Life.Grid   (Grid (..), getCell)
import           Life.Rule   (Rule, born, survives)
import           Life.Stream (Stream, headS, iterateS, splitAtS)
import           Life.Types  (Cell (..), Coord, fromAlive, isAlive)

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

-- | Infinite stream of grids: @g0 :> step r g0 :> step r (step r g0) :> …@.
generations :: Rule -> Grid -> Stream Grid
generations rule = iterateS (step rule)

-- | Apply @step@ @n@ times (@n <= 0@ leaves the grid unchanged), via 'generations'.
stepN :: Int -> Rule -> Grid -> Grid
stepN n rule g
  | n <= 0    = g
  | otherwise = headS (snd (splitAtS n (generations rule g)))
