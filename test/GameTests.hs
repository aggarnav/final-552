module GameTests (test_all, qc) where

import ChessParser
import ChessGame
import ChessSyntax
import Test.HUnit
import Test.QuickCheck
import Control.Monad.State qualified as S
import Data.Map qualified as Map


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
instance Arbitrary Square where
  arbitrary = do
    rank <- elements [1 .. 8]
    file <- elements ['a' .. 'h']
    return $ Square rank file

newtype ArbBoard = ArbBoard Board deriving (Show)

instance Arbitrary ArbBoard where
  arbitrary :: Gen ArbBoard
  arbitrary = do
    b0 <- helper 8 Pawn (ArbBoard Map.empty)
    b1 <- helper 2 Bishop b0
    b2 <- helper 2 Rook b1
    b3 <- helper 2 Knight b2
    b4 <- helper 1 Queen b3
    b5 <- addPiece 1 (CPiece White King) b4
    addPiece 1 (CPiece Black King) b5
    where
      helper :: Int -> Piece -> ArbBoard -> Gen ArbBoard
      helper count p b = do
        i' <- chooseInt (0, count)
        b' <- addPiece i' (CPiece White p) b
        i'' <- chooseInt (0, count)
        addPiece i'' (CPiece Black p) b'
      addPiece :: Int -> CPiece -> ArbBoard -> Gen ArbBoard
      addPiece i piece (ArbBoard board) = do
        if i == 0 then return (ArbBoard board)
        else do
          square <- (arbitrary :: Gen Square)
          addPiece (i-1) piece (ArbBoard (Map.insert square piece board))


instance Arbitrary Game where
  arbitrary = do
    (ArbBoard board') <- (arbitrary :: Gen ArbBoard)
    color <- elements [White, Black] 
    return $ Game board' color

instance Arbitrary CPiece where
  arbitrary = do
    color <- elements [White, Black]
    piece <- elements [Pawn, Knight, Bishop, Rook, Queen, King]
    return $ CPiece color piece

instance Arbitrary Move where
  arbitrary = do
    piece <- elements [Pawn, Knight, Bishop, Rook, Queen, King]
    toSquare <- (arbitrary :: Gen Square)
    return $ NormalMove piece toSquare Nothing (Promotion Nothing) 
      (Capture False) (Check False) (Mate False)

-- Check if the game board changes after a move
prop_validMove :: Game -> Move -> Property
prop_validMove game@(Game board color) move =
  validBoard board && validMove move game ==>
  S.execState (playMove move) game /= game

-- Check that the game board doesn't change after an invalid move
prop_inValidMove :: Game -> Move -> Property
prop_inValidMove game@(Game board color) move =
  validBoard board && not (validMove move game) 
    ==> S.execState (playMove move) game == game

qc :: IO ()
qc = do 
  putStrLn "QuickCheck valid moves"
  quickCheck prop_validMove
  putStrLn "QuickCheck invalid moves"
  quickCheck prop_inValidMove
