module GameTests (test_all, qc) where

import ChessGame
  ( activePlayers,
    initialGame,
    playMoves,
    validBoard,
    validMove,
  )
import ChessParser (parseFile)
import ChessSyntax
  ( Color (Black, White),
    Game (Game),
    Move (Resign),
    MoveResult (InvalidMove, Won),
  )
import Test.HUnit
  ( Assertion,
    Counts,
    Test (TestCase, TestList),
    assert,
    runTestTT,
    (~:),
    (~?=),
  )
import Test.QuickCheck

test_all :: IO Counts
test_all = runTestTT $ TestList [test_valid, test_invalid]

-- Check against most popular games from chessgames.com
test_valid :: Test
test_valid =
  TestList
    [ "game0" ~: checkFile "test/games/game0" (Won White),
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
    [ "badGame0" ~: checkFile "test/games/badGame0" InvalidMove,
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
    Right moves ->
      assert
        ( expected
            == actual
        )
      where
        (actual, _) = playMoves moves initialGame

-- Check if the board changes after a move, is stil valid,
-- along with the player changing and pieces not disappearing
prop_validMove :: Game -> Move -> Property
prop_validMove game@(Game board color) move =
  (move /= Resign)
    && validBoard board
    && validMove move game
    ==> let (_, g@(Game b c)) = playMoves [move] game
         in g /= game
              && validBoard b
              && c /= color
              && activePlayers b <= activePlayers board
              && activePlayers b >= activePlayers board - 1

-- Check that the game board doesn't change after an invalid move
prop_inValidMove :: Game -> Move -> Property
prop_inValidMove game@(Game board color) move =
  validBoard board
    && not (validMove move game)
    ==> actual
    == game
  where
    (_, actual) = playMoves [move] game

-- QuickCheck tests
qc :: IO ()
qc = do
  putStrLn "QuickCheck valid moves"
  quickCheck prop_validMove
  putStrLn "QuickCheck invalid moves"
  quickCheck prop_inValidMove