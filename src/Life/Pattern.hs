-- | Named Life patterns (relative coords) and placement onto a grid.
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

-- | A named set of alive cells in local coordinates (origin top-left of bbox).
data Pattern = Pattern
  { patternName  :: !String
  , patternCells :: ![Coord]
  } deriving (Eq, Show)

-- | Inclusive width/height needed to hold all relative cells (at least 1×1).
patternExtent :: Pattern -> (Width, Height)
patternExtent (Pattern _ []) = (1, 1)
patternExtent (Pattern _ cs) =
  let xs = map fst cs
      ys = map snd cs
  in (maximum xs - minimum xs + 1, maximum ys - minimum ys + 1)

-- | Paint pattern cells as Alive at @offset@; out-of-bounds cells are clipped
-- (via 'setCell'). Does not clear the rest of the grid.
placeOn :: Pattern -> Coord -> Grid -> Grid
placeOn (Pattern _ cs) (ox, oy) g =
  foldl' (\g' (dx, dy) -> setCell g' (ox + dx, oy + dy) Alive) g cs

-- | Empty grid then place pattern at @offset@.
stampPattern :: Pattern -> Coord -> Width -> Height -> Grid
stampPattern p off w h = placeOn p off (emptyGrid w h)

-- | Empty grid then place pattern centred (clipped if larger than the grid).
stampCentered :: Pattern -> Width -> Height -> Grid
stampCentered p w h =
  let (pw, ph) = patternExtent p
      ox = max 0 ((w - pw) `div` 2)
      oy = max 0 ((h - ph) `div` 2)
  in stampPattern p (ox, oy) w h

-- | Built-in catalog for SDL cycling (order stable).
catalog :: [Pattern]
catalog =
  [ gliderP
  , blinkerP
  , toadP
  , beaconP
  , pulsarP
  , lwssP
  ]

-- | Classic glider (facing down-right).
gliderP :: Pattern
gliderP =
  Pattern "glider"
    [ (1, 0)
    , (2, 1)
    , (0, 2), (1, 2), (2, 2)
    ]

-- | Horizontal blinker (period 2).
blinkerP :: Pattern
blinkerP =
  Pattern "blinker"
    [ (0, 1), (1, 1), (2, 1) ]

-- | Toad (period 2).
toadP :: Pattern
toadP =
  Pattern "toad"
    [ (1, 0), (2, 0), (3, 0)
    , (0, 1), (1, 1), (2, 1)
    ]

-- | Beacon (period 2).
beaconP :: Pattern
beaconP =
  Pattern "beacon"
    [ (0, 0), (1, 0)
    , (0, 1), (1, 1)
    , (2, 2), (3, 2)
    , (2, 3), (3, 3)
    ]

-- | Pulsar (period 3); bounding box 13×13.
pulsarP :: Pattern
pulsarP =
  Pattern "pulsar" $
    concat
      [ [ (x, y) | x <- [2, 3, 4, 8, 9, 10], y <- [0, 5, 7, 12] ]
      , [ (x, y) | x <- [0, 5, 7, 12], y <- [2, 3, 4, 8, 9, 10] ]
      ]

-- | Lightweight spaceship facing right.
lwssP :: Pattern
lwssP =
  Pattern "lwss"
    [ (1, 0), (4, 0)
    , (0, 1)
    , (0, 2), (4, 2)
    , (0, 3), (1, 3), (2, 3), (3, 3)
    ]

-- | Convenience: glider stamped near the origin on an empty grid
-- (same layout as the former 'Life.Grid.glider').
glider :: Width -> Height -> Grid
glider w h = stampPattern gliderP (0, 0) w h
