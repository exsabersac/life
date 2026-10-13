-- | 命名生命图案（相对坐标）及落到网格上的放置。
module Life.Pattern
  ( Pattern (..)
  , patternExtent
  , placeOn
  , stampPattern
  , stampCentered
  , catalog
  , gliderP
  , blinkerP
  , toadP
  , beaconP
  , pulsarP
  , lwssP
  , glider
  ) where

import           Data.List  (foldl')
import           Life.Grid  (Grid, emptyGrid, setCell)
import           Life.Types (Cell (..), Coord, Height, Width)

-- | 一组有名存活单元，局部坐标（原点在包围盒左上）。
data Pattern = Pattern
  { patternName  :: !String
  , patternCells :: ![Coord]
  } deriving (Eq, Show)

-- | 容纳全部相对单元所需的宽高（含端点；至少 1×1）。
patternExtent :: Pattern -> (Width, Height)
patternExtent (Pattern _ []) = (1, 1)
patternExtent (Pattern _ cs) =
  let xs = map fst cs
      ys = map snd cs
  in (maximum xs - minimum xs + 1, maximum ys - minimum ys + 1)

-- | 在 @offset@ 处把图案单元画为 Alive；越界由 'setCell' 裁剪。
-- 不清空网格其余部分。
placeOn :: Pattern -> Coord -> Grid -> Grid
placeOn (Pattern _ cs) (ox, oy) g =
  foldl' (\g' (dx, dy) -> setCell g' (ox + dx, oy + dy) Alive) g cs

-- | 空网格后在 @offset@ 放置图案。
stampPattern :: Pattern -> Coord -> Width -> Height -> Grid
stampPattern p off w h = placeOn p off (emptyGrid w h)

-- | 空网格后居中放置（图案大于网格时会裁剪）。
stampCentered :: Pattern -> Width -> Height -> Grid
stampCentered p w h =
  let (pw, ph) = patternExtent p
      ox = max 0 ((w - pw) `div` 2)
      oy = max 0 ((h - ph) `div` 2)
  in stampPattern p (ox, oy) w h

-- | SDL 循环用的内置目录（顺序稳定）。
catalog :: [Pattern]
catalog =
  [ gliderP
  , blinkerP
  , toadP
  , beaconP
  , pulsarP
  , lwssP
  ]

-- | 经典滑翔机（朝右下）。
gliderP :: Pattern
gliderP =
  Pattern "glider"
    [ (1, 0)
    , (2, 1)
    , (0, 2), (1, 2), (2, 2)
    ]

-- | 水平闪烁器（周期 2）。
blinkerP :: Pattern
blinkerP =
  Pattern "blinker"
    [ (0, 1), (1, 1), (2, 1) ]

-- | 蟾蜍（周期 2）。
toadP :: Pattern
toadP =
  Pattern "toad"
    [ (1, 0), (2, 0), (3, 0)
    , (0, 1), (1, 1), (2, 1)
    ]

-- | 信标（周期 2）。
beaconP :: Pattern
beaconP =
  Pattern "beacon"
    [ (0, 0), (1, 0)
    , (0, 1), (1, 1)
    , (2, 2), (3, 2)
    , (2, 3), (3, 3)
    ]

-- | 脉冲星（周期 3）；包围盒 13×13。
pulsarP :: Pattern
pulsarP =
  Pattern "pulsar" $
    concat
      [ [ (x, y) | x <- [2, 3, 4, 8, 9, 10], y <- [0, 5, 7, 12] ]
      , [ (x, y) | x <- [0, 5, 7, 12], y <- [2, 3, 4, 8, 9, 10] ]
      ]

-- | 轻型飞船（朝右）。
lwssP :: Pattern
lwssP =
  Pattern "lwss"
    [ (1, 0), (4, 0)
    , (0, 1)
    , (0, 2), (4, 2)
    , (0, 3), (1, 3), (2, 3), (3, 3)
    ]

-- | 便捷：空网格上靠近原点盖一艘滑翔机
-- （布局与原先 'Life.Grid.glider' 相同）。
glider :: Width -> Height -> Grid
glider w h = stampPattern gliderP (0, 0) w h
