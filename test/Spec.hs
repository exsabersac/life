{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.List       (sort)
import           Test.Tasty      (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Life.Grid
import           Life.Rule
import           Life.Step
import           Life.Types      (Cell (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "life"
  [ ruleTests
  , gridTests
  , stepTests
  , qcTests
  ]

ruleTests :: TestTree
ruleTests = testGroup "Rule"
  [ testCase "parse Conway B3/S23" $
      parseRule "B3/S23" @?= Right conway
  , testCase "parse HighLife case-insensitive" $
      parseRule "b36/s23" @?= Right highLife
  , testCase "parse Seeds B2/S" $
      parseRule "B2/S" @?= Right seeds
  , testCase "parse empty survive" $
      parseRule "B2/S" @?= Right (Rule (birth seeds) (survive seeds))
  , testCase "reject garbage" $
      case parseRule "not-a-rule" of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left"
  , testCase "ruleToString roundtrip Conway" $
      parseRule (ruleToString conway) @?= Right conway
  , testCase "born/survives Conway" $ do
      assertBool "B3" (born conway 3)
      assertBool "not B2" (not (born conway 2))
      assertBool "S2" (survives conway 2)
      assertBool "S3" (survives conway 3)
      assertBool "not S1" (not (survives conway 1))
  ]

gridTests :: TestTree
gridTests = testGroup "Grid"
  [ testCase "empty is all dead" $
      aliveCoords (emptyGrid 3 3) @?= []
  , testCase "dead border out of bounds" $
      getCell (emptyGrid 2 2) (-1, 0) @?= Dead
  , testCase "toggle and set" $ do
      let g0 = emptyGrid 4 4
          g1 = setCell g0 (1, 1) Alive
          g2 = toggleCell g1 (1, 1)
      getCell g1 (1, 1) @?= Alive
      getCell g2 (1, 1) @?= Dead
  , testCase "glider has 5 live cells" $
      length (aliveCoords (glider 10 10)) @?= 5
  ]

stepTests :: TestTree
stepTests = testGroup "Step"
  [ testCase "empty stays empty" $
      step conway (emptyGrid 5 5) @?= emptyGrid 5 5
  , testCase "lone cell dies" $ do
      let g = fromAliveCoords 5 5 [(2, 2)]
      aliveCoords (step conway g) @?= []
  , testCase "block is still life" $ do
      let g = fromAliveCoords 6 6 [(2, 2), (3, 2), (2, 3), (3, 3)]
      sort (aliveCoords (step conway g)) @?= sort (aliveCoords g)
  , testCase "blinker period 2" $ do
      let horizontal = fromAliveCoords 5 5 [(1, 2), (2, 2), (3, 2)]
          vertical   = fromAliveCoords 5 5 [(2, 1), (2, 2), (2, 3)]
      sort (aliveCoords (step conway horizontal)) @?= sort (aliveCoords vertical)
      sort (aliveCoords (stepN 2 conway horizontal)) @?= sort (aliveCoords horizontal)
  , testCase "neighborCount centre of block" $ do
      let g = fromAliveCoords 6 6 [(2, 2), (3, 2), (2, 3), (3, 3)]
      neighborCount g (2, 2) @?= 3
  , testCase "edge cell sees dead border" $ do
      let g = fromAliveCoords 3 3 [(0, 0)]
      neighborCount g (0, 0) @?= 0
  , testCase "glider translates under Conway (4 gens)" $ do
      let g0 = glider 20 20
          g4 = stepN 4 conway g0
          shifted = fromAliveCoords 20 20 [(x + 1, y + 1) | (x, y) <- aliveCoords g0]
      sort (aliveCoords g4) @?= sort (aliveCoords shifted)
  ]

qcTests :: TestTree
qcTests = testGroup "QuickCheck"
  [ testProperty "step preserves dimensions" $ \xs ->
      let cells = take 64 (map (\b -> if b then Alive else Dead) (xs :: [Bool]))
                  ++ replicate (64 - min 64 (length xs)) Dead
          g = case mkGrid 8 8 (take 64 cells) of
                Just grid -> grid
                Nothing   -> emptyGrid 8 8
          g' = step conway g
      in gridWidth g' == 8 && gridHeight g' == 8
  , testProperty "parse ruleToString idempotent for presets" $
      forAll (elements (map snd presets)) $ \r ->
        parseRule (ruleToString r) == Right r
  ]
