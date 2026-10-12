{-# LANGUAGE OverloadedStrings #-}
-- | LifeWiki-style plaintext @.cells@ encoding ( @.@ / @O@ / @*@ , @#@/@!@ comments).
module Life.Cells
  ( encodeCells
  , decodeCells
  ) where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Life.Grid (Grid, getCell, gridHeight, gridWidth, mkGrid)
import           Life.Types (Cell (..))

-- | Encode the full grid as @.cells@ rows (@O@ alive, @.@ dead), with a short header.
encodeCells :: Grid -> Text
encodeCells g =
  let w = gridWidth g
      h = gridHeight g
      header =
        [ "# life .cells"
        , "#W " <> T.pack (show w)
        , "#H " <> T.pack (show h)
        ]
      rows =
        [ T.pack
            [ cellChar (getCell g (x, y))
            | x <- [0 .. w - 1]
            ]
        | y <- [0 .. h - 1]
        ]
  in T.unlines (header ++ rows)
  where
    cellChar Alive = 'O'
    cellChar Dead  = '.'

-- | Decode @.cells@ text into a grid.
--
-- Skips blank lines and comments (@#@ / @!@). Data lines use @.@ (dead) and
-- @O@/@*@ (alive); other non-space characters are rejected. Shorter rows are
-- padded with dead cells to the maximum row width. Optional @#W@ / @#H@ comments
-- set dimensions when there is no cell data (all-dead grid) or enlarge the canvas.
decodeCells :: Text -> Either String Grid
decodeCells txt = do
  let rawLines = T.lines txt
      (metaW, metaH, dataLines) = foldl parseLine (Nothing, Nothing, []) rawLines
  case dataLines of
    [] ->
      case (metaW, metaH) of
        (Just w, Just h)
          | w > 0 && h > 0 ->
              maybe (Left "mkGrid failed") Right (mkGrid w h (replicate (w * h) Dead))
          | otherwise -> Left "invalid #W/#H"
        _ -> Left "no cell data"
    rows -> do
      parsed <- traverse parseDataRow rows
      let maxW = maximum (map length parsed)
          hData = length parsed
          w = maybe maxW (max maxW) metaW
          h = maybe hData (max hData) metaH
      checkDims w h
      let padded =
            [ row ++ replicate (w - length row) Dead
            | row <- parsed
            ]
              ++ replicate (h - hData) (replicate w Dead)
      maybe (Left "mkGrid failed") Right (mkGrid w h (concat padded))
  where
    checkDims w h
      | w <= 0 || h <= 0 = Left "grid dimensions must be positive"
      | otherwise        = Right ()

    parseLine :: (Maybe Int, Maybe Int, [Text]) -> Text -> (Maybe Int, Maybe Int, [Text])
    parseLine (mw, mh, acc) line =
      let t = T.stripEnd line
      in if T.null (T.strip t)
           then (mw, mh, acc)
           else case T.uncons t of
             Just ('#', rest) ->
               let r = T.strip rest
               in if T.isPrefixOf "W " r || T.isPrefixOf "w " r
                    then (readDim (T.drop 2 r) mw, mh, acc)
                    else if T.isPrefixOf "H " r || T.isPrefixOf "h " r
                           then (mw, readDim (T.drop 2 r) mh, acc)
                           else (mw, mh, acc)
             Just ('!', _) -> (mw, mh, acc)
             _ -> (mw, mh, acc ++ [t])

    readDim :: Text -> Maybe Int -> Maybe Int
    readDim s fallback =
      case reads (T.unpack (T.strip s)) :: [(Int, String)] of
        [(n, "")] | n > 0 -> Just n
        _                 -> fallback

    parseDataRow :: Text -> Either String [Cell]
    parseDataRow row =
      traverse parseChar (filter (not . isSpace) (T.unpack row))

    parseChar :: Char -> Either String Cell
    parseChar '.' = Right Dead
    parseChar 'O' = Right Alive
    parseChar 'o' = Right Alive
    parseChar '*' = Right Alive
    parseChar c   = Left $ "invalid cell character: " ++ show c
