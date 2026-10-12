{-# LANGUAGE OverloadedStrings #-}
-- | Birth / Survive (B/S) rule parsing and lookup.
--
-- Examples: @B3/S23@ (Conway), @B36/S23@ (HighLife), @B2/S@ (Seeds).
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

-- | Moore-neighbour B/S rule: birth counts and survival counts (0–8).
data Rule = Rule
  { birth   :: !(Set Int)
  , survive :: !(Set Int)
  } deriving (Eq, Show)

born :: Rule -> Int -> Bool
born r n = n `Set.member` birth r

survives :: Rule -> Int -> Bool
survives r n = n `Set.member` survive r

conway :: Rule
conway = Rule (Set.fromList [3]) (Set.fromList [2, 3])

highLife :: Rule
highLife = Rule (Set.fromList [3, 6]) (Set.fromList [2, 3])

seeds :: Rule
seeds = Rule (Set.fromList [2]) Set.empty

-- | Named presets for the SDL shell (cycle with a key).
presets :: [(String, Rule)]
presets =
  [ ("B3/S23",  conway)
  , ("B36/S23", highLife)
  , ("B2/S",    seeds)
  ]

ruleToString :: Rule -> String
ruleToString (Rule b s) =
  'B' : digits b ++ '/' : 'S' : digits s
  where
    digits = concatMap show . Set.toAscList

parseRuleText :: Text -> Either String Rule
parseRuleText = parseRule . T.unpack

-- | Parse @B\<digits\>/S\<digits\>@ (letters case-insensitive; digit lists may be empty).
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
