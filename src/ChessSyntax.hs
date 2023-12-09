module ChessSyntax where

import Data.Char (toLower)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    chooseInt,
    elements,
    frequency,
  )

-- Using https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
-- Details the moves from Parser

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq)

data Color = White | Black deriving (Eq, Show)

data CPiece = CPiece Color Piece deriving (Eq)

type File = Char

type Rank = Int

data Square = Square Rank File deriving (Eq, Show)

newtype Promotion = Promotion (Maybe Piece) deriving (Eq, Show)

newtype Capture = Capture Bool deriving (Eq, Show)

newtype Check = Check Bool deriving (Eq, Show)

newtype Mate = Mate Bool deriving (Eq, Show)

type Board = Map Square CPiece

data Game = Game Board Color deriving (Eq, Show)

data MoveResult = Won Color | ContinueGame | InvalidMove
  deriving (Eq, Show)

-- the square is the destination
data Move
  = NormalMove
      Piece
      Square
      (Maybe Disambiguation)
      Promotion
      Capture
      Check
      Mate
  | KingSideCastling
  | QueenSideCastling
  | Resign
  deriving (Eq, Show)

data Disambiguation
  = File File
  | Rank Rank
  | Both Square
  deriving (Show, Eq)

-------------------------
---Instance Definition---
-------------------------
instance Ord Square where
  compare :: Square -> Square -> Ordering
  compare (Square f1 r1) (Square f2 r2) = compare (r1, f1) (r2, f2)

instance Show Piece where
  show :: Piece -> String
  show Pawn = "P"
  show Knight = "N"
  show Bishop = "B"
  show Rook = "R"
  show Queen = "Q"
  show King = "K"

instance Show CPiece where
  show :: CPiece -> String
  show (CPiece White p) = show p
  show (CPiece Black p) = map toLower $ show p

-------------------------
-- Arbitrary definitions--
-------------------------

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = elements [White, Black]

instance Arbitrary Square where
  arbitrary :: Gen Square
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
        if i == 0
          then return (ArbBoard board)
          else do
            square <- (arbitrary :: Gen Square)
            addPiece (i - 1) piece (ArbBoard (Map.insert square piece board))

instance Arbitrary Game where
  arbitrary :: Gen Game
  arbitrary = do
    (ArbBoard board') <- (arbitrary :: Gen ArbBoard)
    Game board' <$> arbitrary

instance Arbitrary Piece where
  arbitrary :: Gen Piece
  arbitrary = elements [Pawn, Knight, Bishop, Rook, Queen, King]

instance Arbitrary CPiece where
  arbitrary :: Gen CPiece
  arbitrary = do
    color <- arbitrary
    CPiece color <$> arbitrary

instance Arbitrary Disambiguation where
  arbitrary :: Gen Disambiguation
  arbitrary =
    frequency
      [ (1, File <$> elements ['a' .. 'h']),
        (1, Rank <$> elements [1 .. 8]),
        (1, Both <$> (arbitrary :: Gen Square))
      ]

instance Arbitrary Move where
  arbitrary :: Gen Move
  arbitrary =
    frequency
      [ (97, genNormalMove),
        (1, return KingSideCastling),
        (1, return QueenSideCastling),
        (1, return Resign)
      ]
    where
      genNormalMove :: Gen Move
      genNormalMove = do
        piece <- elements [Pawn, Knight, Bishop, Rook, Queen, King]
        toSquare <- (arbitrary :: Gen Square)
        cap <- frequency [(1, return True), (9, return False)]
        check <- frequency [(1, return True), (9, return False)]
        checkm <- frequency [(1, return True), (50, return False)]
        prom <- frequency [(1, Just <$> (arbitrary :: Gen Piece)), (50, return Nothing)]
        disam <-
          frequency
            [ (1, Just <$> (arbitrary :: Gen Disambiguation)),
              (50, return Nothing)
            ]
        return $
          NormalMove
            piece
            toSquare
            disam
            (Promotion prom)
            (Capture cap)
            (Check check)
            (Mate checkm)
