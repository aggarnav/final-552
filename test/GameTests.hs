module GameTests (test_all) where

import ChessParser
import ChessSyntax
import Test.HUnit
import Test.QuickCheck

test_all :: IO Counts
test_all = runTestTT test_parseMoves

test_parseMoves :: Test
test_parseMoves =
  TestList
    [ -- Test Pawn move
      parseMoves "e3" ~?= Right [NormalMove Pawn (Square 3 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test Pawn move with disambiguation
      parseMoves "e7e3" ~?= Right [NormalMove Pawn (Square 3 'e') (Just (Both (Square 7 'e'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test King move
      parseMoves "Ka1b4" ~?= Right [NormalMove King (Square 4 'b') (Just (Both (Square 1 'a'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test Castling
      parseMoves "O-O" ~?= Right [KingSideCastling],
      parseMoves "O-O-O" ~?= Right [QueenSideCastling],
      -- Test Pawn Promotion
      parseMoves "ae7=Q" ~?= Right [NormalMove Pawn (Square 7 'e') (Just (File 'a')) (Promotion (Just Queen)) (Capture False) (Check False) (Mate False)],
      -- Test multiple moves parsing
      parseMoves "e3e4 e3"
        ~?= Right
          [ NormalMove Pawn (Square 4 'e') (Just (Both (Square 3 'e'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 3 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ]
    ]

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
