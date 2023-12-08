module GameTests (test_all) where

import ChessParser
import ChessGame
import ChessSyntax
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State qualified as S


test_all :: IO Counts
test_all = runTestTT test_parseGames

test_parseGames :: Test
test_parseGames =
  TestList
    [ 
      "game1" ~: p "test/game1" (Won White),
      "game2" ~: p "test/game2" (Won White),
      "game3" ~: p "test/game3" (Won White),
      "game4" ~: p "test/game4" (Won White),
      "game5" ~: p "test/game5" (Won White),
      "game6" ~: p "test/game6" (Won White),
      "game7" ~: p "test/game7" (Won White),
      "game8" ~: p "test/game8" (Won White),
      "game9" ~: p "test/game9" (Won White),
      "game10" ~: p "test/game10" (Won White)
    ]
  where
    p fn expected = do
      putStrLn ("Parsing " ++ fn)
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
