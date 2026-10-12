{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Data.List       (sort)
import qualified Data.Text       as T
import           Test.Tasty      (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Life.Cells
import           Life.Grid
import           Life.Pattern
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
  , patternTests
  , cellsTests
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

patternTests :: TestTree
patternTests = testGroup "Pattern"
  [ testCase "catalog names" $
      map patternName catalog
        @?= ["glider", "blinker", "toad", "beacon", "pulsar", "lwss"]
  , testCase "gliderP alive count" $
      length (patternCells gliderP) @?= 5
  , testCase "blinker stamped oscillates period 2" $ do
      let g0 = stampPattern blinkerP (1, 1) 7 7
          g2 = stepN 2 conway g0
      sort (aliveCoords g2) @?= sort (aliveCoords g0)
      length (aliveCoords g0) @?= 3
  , testCase "toad period 2" $ do
      let g0 = stampCentered toadP 10 10
      sort (aliveCoords (stepN 2 conway g0)) @?= sort (aliveCoords g0)
  , testCase "beacon period 2" $ do
      let g0 = stampCentered beaconP 10 10
      sort (aliveCoords (stepN 2 conway g0)) @?= sort (aliveCoords g0)
  , testCase "pulsar has 48 cells and period 3" $ do
      length (patternCells pulsarP) @?= 48
      let g0 = stampCentered pulsarP 20 20
          g3 = stepN 3 conway g0
      length (aliveCoords g0) @?= 48
      sort (aliveCoords g3) @?= sort (aliveCoords g0)
  , testCase "lwss has 9 cells" $
      length (patternCells lwssP) @?= 9
  , testCase "placeOn clips out of bounds" $ do
      let g = placeOn gliderP (8, 8) (emptyGrid 10 10)
      -- only cells that land inside survive
      assertBool "some cells placed" (not (null (aliveCoords g)))
      assertBool "all in bounds" (all (\(x, y) -> x >= 0 && x < 10 && y >= 0 && y < 10) (aliveCoords g))
  , testCase "pulsar on tiny grid clips cleanly" $ do
      let g = stampCentered pulsarP 5 5
      assertBool "width preserved" (gridWidth g == 5)
      assertBool "fewer than 48 fit" (length (aliveCoords g) < 48)
  ]

cellsTests :: TestTree
cellsTests = testGroup "Cells"
  [ testCase "round-trip empty grid" $ do
      let g = emptyGrid 4 3
      decodeCells (encodeCells g) @?= Right g
  , testCase "round-trip glider" $ do
      let g = glider 10 8
      decodeCells (encodeCells g) @?= Right g
  , testCase "round-trip pulsar stamp" $ do
      let g = stampCentered pulsarP 20 20
      decodeCells (encodeCells g) @?= Right g
  , testCase "accept * as alive" $
      case decodeCells "*.\n.*" of
        Right g -> sort (aliveCoords g) @?= [(0, 0), (1, 1)]
        Left e  -> assertFailure e
  , testCase "skip comments and blanks" $
      case decodeCells "# hi\n\n!name\n.O.\n..O\nOOO\n" of
        Right g -> sort (aliveCoords g) @?= sort (aliveCoords (glider 3 3))
        Left e  -> assertFailure e
  , testCase "pad short rows" $
      case decodeCells "O\n.O\n" of
        Right g -> do
          gridWidth g @?= 2
          gridHeight g @?= 2
          sort (aliveCoords g) @?= [(0, 0), (1, 1)]
        Left e -> assertFailure e
  , testCase "#W #H only yields empty grid" $
      case decodeCells "#W 3\n#H 2\n" of
        Right g -> do
          gridWidth g @?= 3
          gridHeight g @?= 2
          aliveCoords g @?= []
        Left e -> assertFailure e
  , testCase "reject empty input" $
      case decodeCells "" of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left"
  , testCase "reject bad character" $
      case decodeCells "OXO" of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left"
  , testCase "encode header mentions size" $ do
      let t = encodeCells (emptyGrid 5 4)
      assertBool "has W" (T.isInfixOf "#W 5" t)
      assertBool "has H" (T.isInfixOf "#H 4" t)
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
  , testProperty "encode/decode round-trip random 6x6" $ \xs ->
      let cells = take 36 (map (\b -> if b then Alive else Dead) (xs :: [Bool]))
                  ++ replicate (36 - min 36 (length xs)) Dead
          g = case mkGrid 6 6 (take 36 cells) of
                Just grid -> grid
                Nothing   -> emptyGrid 6 6
      in decodeCells (encodeCells g) == Right g
  ]
