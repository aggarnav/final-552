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
      "game1" ~: p "test/games/game0" (Won White),
      "game1" ~: p "test/games/game1" (Won White),
      "game2" ~: p "test/games/game2" (Won White),
      "game3" ~: p "test/games/game3" (Won Black),
      "game4" ~: p "test/games/game4" (Won White),
      "game5" ~: p "test/games/game5" (Won Black),
      "game6" ~: p "test/games/game6" (Won Black),
      "game7" ~: p "test/games/game7" (Won Black),
      "game8" ~: p "test/games/game8" (Won White),
      "game9" ~: p "test/games/game9" (Won White)
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
