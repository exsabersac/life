-- | 有限网格上一世代步进（B/S 规则、死边界），以及无穷世代流。
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

-- | 八邻域（Moore）存活数；越界视为死（死边界）。
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

-- | 推进一世代。新数组按 @Ix@ 顺序经 @range@ 构建。
step :: Rule -> Grid -> Grid
step rule g =
  let bnds = bounds (gridCells g)
      cells = [(c, stepCell rule g c) | c <- range bnds]
  in Grid (array bnds cells)

-- | 无穷网格流：@g0 :> step r g0 :> step r (step r g0) :> …@。
--
-- 用 'iterateS' 构造；消费时配合 'splitAtS' / 'headS'，只强制已取前缀。
generations :: Rule -> Grid -> Stream Grid
generations rule = iterateS (step rule)

-- | 连续应用 @step@ @n@ 次（@n <= 0@ 不改网格），经 'generations' 实现。
stepN :: Int -> Rule -> Grid -> Grid
stepN n rule g
  | n <= 0    = g
  | otherwise = headS (snd (splitAtS n (generations rule g)))
