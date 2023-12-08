module GameTests (test_all) where

import ChessParser
import ChessGame
import ChessSyntax
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State qualified as S


test_all :: IO Counts
test_all = runTestTT $ TestList [test_valid, test_invalid]

-- Check against most popular games from chessgames.com
test_valid :: Test
test_valid =
  TestList
    [ 
      "game0" ~: checkFile "test/games/game0" (Won White),
      "game1" ~: checkFile "test/games/game1" (Won White),
      "game2" ~: checkFile "test/games/game2" (Won White),
      "game3" ~: checkFile "test/games/game3" (Won Black),
      "game4" ~: checkFile "test/games/game4" (Won White),
      "game5" ~: checkFile "test/games/game5" (Won Black),
      "game6" ~: checkFile "test/games/game6" (Won Black),
      "game7" ~: checkFile "test/games/game7" (Won Black),
      "game8" ~: checkFile "test/games/game8" (Won White),
      "game9" ~: checkFile "test/games/game9" (Won White)
    ]

-- Check against most popular games from chessgames.com
test_invalid :: Test
test_invalid =
  TestList 
    [ 
      "badGame0" ~: checkFile "test/games/badGame0" InvalidMove,
      "badGame1" ~: checkFile "test/games/badGame1" InvalidMove,
      "badGame2" ~: checkFile "test/games/badGame2" InvalidMove,
      "badGame3" ~: checkFile "test/games/badGame3" InvalidMove,
      "badGame4" ~: checkFile "test/games/badGame4" InvalidMove,
      "badGame5" ~: checkFile "test/games/badGame5" InvalidMove,
      "badGame6" ~: checkFile "test/games/badGame6" InvalidMove,
      "badGame7" ~: checkFile "test/games/badGame7" InvalidMove,
      "badGame8" ~: checkFile "test/games/badGame8" InvalidMove,
      "badGame9" ~: checkFile "test/games/badGame9" InvalidMove
    ]

-- helper to run games from files
checkFile :: String -> MoveResult -> Assertion
checkFile fn expected = do
  result <- parseFile fn
  case result of 
    Left err -> assert False
    Right moves -> assert (expected == S.evalState 
      (playMoves moves) initialGame)

-------------------------
-- Arbitrary definitions--
-------------------------
-- instance Arbitrary Game where
--   arbitrary = undefined

-- instance Arbitrary Move where
--   arbitrary = undefined

-- Check if the game board changes after a move
-- prop_validMove :: Game -> Move -> Property
-- prop_validMove game move =
--   runState (validMove move) game
--     ==> runState (playMove move) game
--     /= game

-- Check that the game board doesn't change after an invalid move
