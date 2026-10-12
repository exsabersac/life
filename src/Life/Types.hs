-- | Shared types for the cellular automata core.
module Life.Types
  ( Cell (..)
  , isAlive
  , fromAlive
  , Coord
  , Width
  , Height
  ) where

-- | Binary cell state (MVP: dead / alive).
data Cell
  = Dead
  | Alive
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive Dead  = False

fromAlive :: Bool -> Cell
fromAlive True  = Alive
fromAlive False = Dead

-- | Column (x) then row (y), zero-based, origin at top-left.
type Coord = (Int, Int)

type Width = Int
type Height = Int
