{-# LANGUAGE OverloadedStrings #-}
-- | LifeWiki 风格明文 @.cells@ 编解码（@.@ / @O@ / @*@，@#@/@!@ 注释）。
--
-- 写出时附带 @#W@ / @#H@，空盘也能往返。
module Life.Cells
  ( encodeCells
  , decodeCells
  ) where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Life.Grid (Grid, getCell, gridHeight, gridWidth, mkGrid)
import           Life.Types (Cell (..))

-- | 将整盘编码为 @.cells@ 行（@O@ 活、@.@ 死），带简短文件头。
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

-- | 将 @.cells@ 文本解码为网格。
--
-- 跳过空行与注释（@#@ / @!@）。数据行：@.@ 死，@O@/@*@ 活；其它非空白字符报错。
-- 较短行用死单元补齐到最大行宽。可选 @#W@ / @#H@ 在无数据（全死盘）时定尺寸，
-- 或扩大画布。
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
