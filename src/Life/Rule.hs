{-# LANGUAGE OverloadedStrings #-}
-- | 出生 / 存活（B/S）规则解析与查询。
--
-- 示例：@B3/S23@（Conway）、@B36/S23@（HighLife）、@B2/S@（Seeds）。
module Life.Rule
  ( Rule (..)
  , parseRule
  , parseRuleText
  , ruleToString
  , born
  , survives
  , conway
  , highLife
  , seeds
  , presets
  ) where

import           Data.Char (digitToInt, isDigit, toUpper)
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Data.Text (Text)
import qualified Data.Text as T

-- | Moore 邻域 B/S 规则：出生邻居数集合与存活邻居数集合（0–8）。
data Rule = Rule
  { birth   :: !(Set Int)
  , survive :: !(Set Int)
  } deriving (Eq, Show)

-- | 死单元在邻居数 @n@ 时是否出生。
born :: Rule -> Int -> Bool
born r n = n `Set.member` birth r

-- | 活单元在邻居数 @n@ 时是否存活。
survives :: Rule -> Int -> Bool
survives r n = n `Set.member` survive r

-- | Conway 生命游戏：@B3/S23@。
conway :: Rule
conway = Rule (Set.fromList [3]) (Set.fromList [2, 3])

-- | HighLife：@B36/S23@。
highLife :: Rule
highLife = Rule (Set.fromList [3, 6]) (Set.fromList [2, 3])

-- | Seeds：@B2/S@。
seeds :: Rule
seeds = Rule (Set.fromList [2]) Set.empty

-- | SDL 壳循环用的命名预设。
presets :: [(String, Rule)]
presets =
  [ ("B3/S23",  conway)
  , ("B36/S23", highLife)
  , ("B2/S",    seeds)
  ]

-- | 打印为 @B…/S…@ 字符串。
ruleToString :: Rule -> String
ruleToString (Rule b s) =
  'B' : digits b ++ '/' : 'S' : digits s
  where
    digits = concatMap show . Set.toAscList

parseRuleText :: Text -> Either String Rule
parseRuleText = parseRule . T.unpack

-- | 解析 @B\<digits\>/S\<digits\>@（字母大小写不敏感；数字列表可为空）。
parseRule :: String -> Either String Rule
parseRule raw =
  let norm = map toUpper (filter (/= ' ') raw)
  in case break (== '/') norm of
       ('B' : bs, '/' : 'S' : ss) -> do
         b <- parseDigits bs
         s <- parseDigits ss
         pure (Rule b s)
       _ -> Left $ "expected B…/S…, got: " ++ raw

parseDigits :: String -> Either String (Set Int)
parseDigits ds = do
  ns <- traverse digit ds
  if all (\n -> n >= 0 && n <= 8) ns
    then pure (Set.fromList ns)
    else Left "neighbour counts must be 0–8"
  where
    digit d
      | isDigit d = Right (digitToInt d)
      | otherwise = Left $ "non-digit in rule counts: " ++ [d]
