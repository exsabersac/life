-- | 元胞自动机核心的共享类型。
module Life.Types
  ( Cell (..)
  , isAlive
  , fromAlive
  , Coord
  , Width
  , Height
  ) where

-- | 二元单元状态（MVP：死 / 活）。
data Cell
  = Dead
  | Alive
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | 是否为存活单元。
isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead  = False

-- | 布尔转单元（@True@ → Alive）。
fromAlive :: Bool -> Cell
fromAlive True  = Alive
fromAlive False = Dead

-- | 列 (x) 再行 (y)，从零计，原点在左上。
type Coord = (Int, Int)

-- | 网格宽度（列数）。
type Width = Int

-- | 网格高度（行数）。
type Height = Int
