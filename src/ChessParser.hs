module ChessParser
  ( parseMoves,
    parseSingleMove,
    parseFile,
  )
where

import ChessSyntax
import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Maybe (fromMaybe, isJust)
import Test.HUnit (Test (TestCase, TestList), runTestTT, (~:), (~?=))
import Text.Parsec
  ( ParseError,
    char,
    digit,
    eof,
    oneOf,
    option,
    optionMaybe,
    parse,
    spaces,
    string,
    try,
  )
import Text.Parsec.String (Parser, parseFromFile)

-- Given pace separated moves, return a list of Moves
parseMoves :: String -> Either ParseError [Move]
parseMoves = parse movesParser ""

-- Function that actaully parse a single chess move
parseSingleMove :: String -> Either ParseError Move
parseSingleMove = parse moveParser ""

-- Parse a file containing space separated moves
parseFile :: String -> IO (Either ParseError [Move])
parseFile = parseFromFile (const <$> movesParser <*> eof)

-- Space separated moves parser
movesParser :: Parser [Move]
movesParser = some $ do
  move <- moveParser
  spaces
  return move

-- Single move parser
moveParser :: Parser Move
moveParser =
  try resignParser
    <|> try queensideCastlingParser
    <|> try kingsideCastlingParser
    <|> try normalMoveWithDisambiguationParser
    <|> normalMoveParser

-- Normal move parser
normalMoveParser :: Parser Move
normalMoveParser = do
  p <- optionMaybe pieceParser
  capture <- captureParser
  toSquare <- squareParser
  prom <- promotionParser
  check <- optionMaybe (char '+')
  checkmate <- optionMaybe (char '#')
  return $
    NormalMove
      (fromMaybe Pawn p)
      toSquare
      Nothing
      (Promotion prom)
      (Capture capture)
      (Check (isJust check))
      (Mate (isJust checkmate))

-- Normal move with disambiguation parser
normalMoveWithDisambiguationParser :: Parser Move
normalMoveWithDisambiguationParser = do
  p <- optionMaybe pieceParser
  disambiguation <-
    optionMaybe
      ( try bothParser
          <|> try rankParser
          <|> try fileParser
      )
  capture <- captureParser
  toSquare <- squareParser
  prom <- promotionParser
  check <- optionMaybe (char '+')
  checkmate <- optionMaybe (char '#')
  return $
    NormalMove
      (fromMaybe Pawn p)
      toSquare
      disambiguation
      (Promotion prom)
      (Capture capture)
      (Check (isJust check))
      (Mate (isJust checkmate))

-- Piece parser
pieceParser :: Parser Piece
pieceParser = do
  p <- oneOf "NBRQK"
  return $ case p of
    'N' -> Knight
    'B' -> Bishop
    'R' -> Rook
    'Q' -> Queen
    'K' -> King
    _ -> error "Impossible"

-- Square parser
squareParser :: Parser Square
squareParser = do
  file <- oneOf ['a' .. 'h'] -- File (column)
  rank <- oneOf ['1' .. '8'] -- Rank (row)
  return $ Square (read [rank]) file

-- Disambiguation file parser
fileParser :: Parser Disambiguation
fileParser = do
  file <- oneOf ['a' .. 'h']
  return $ File file

-- Disambiguation rank parser
rankParser :: Parser Disambiguation
rankParser = do
  rank <- digit
  return $ Rank (read [rank])

-- Disambiguation both parser
bothParser :: Parser Disambiguation
bothParser = do
  file <- oneOf ['a' .. 'h'] -- File (column)
  rank <- oneOf ['1' .. '8'] -- Rank (row)
  return $ Both $ Square (read [rank]) file

-- Capture parser
captureParser :: Parser Bool
captureParser = option False (char 'x' >> return True)

-- Promotion parser
promotionParser :: Parser (Maybe Piece)
promotionParser = optionMaybe $ do
  char '='
  p <- oneOf "NBRQ"
  return $ case p of
    'N' -> Knight
    'B' -> Bishop
    'R' -> Rook
    'Q' -> Queen
    _ -> error "Impossible"

-- Kingside castling parser
kingsideCastlingParser :: Parser Move
kingsideCastlingParser = do
  try $ string "O-O"
  return KingSideCastling

-- Queenside castling parser
queensideCastlingParser :: Parser Move
queensideCastlingParser = do
  try $ string "O-O-O"
  return QueenSideCastling

-- Resign parser
resignParser :: Parser Move
resignParser = do
  try (string "1-0") <|> try (string "0-1")
  return Resign

pretty :: [Move] -> [Char]
pretty = unwords . map singlePretty

singlePretty :: Move -> String
singlePretty move =
  case move of
    QueenSideCastling -> "O-O-O"
    KingSideCastling -> "O-O"
    Resign -> "1-0"
    NormalMove piece toSqure disambiguation promo capture check mate ->
      pieceToString piece
        ++ disambiguationToString disambiguation
        ++ captureToString capture
        ++ squareToString toSqure
        ++ promotionToString promo
        ++ checkToString check
        ++ mateToString mate

pieceToString :: Piece -> String
pieceToString piece = case piece of
  Pawn -> ""
  Knight -> "N"
  Bishop -> "B"
  Rook -> "R"
  Queen -> "Q"
  King -> "K"

squareToString :: Square -> String
squareToString (Square rank file) = file : show rank

disambiguationToString :: Maybe Disambiguation -> String
disambiguationToString (Just dis) = case dis of
  File f -> [f]
  Rank r -> show r
  Both sq -> squareToString sq
disambiguationToString Nothing = ""

promotionToString :: Promotion -> String
promotionToString (Promotion (Just piece)) = "=" ++ pieceToString piece
promotionToString (Promotion Nothing) = ""

captureToString :: Capture -> String
captureToString (Capture True) = "x"
captureToString (Capture False) = ""

checkToString :: Check -> String
checkToString (Check True) = "+"
checkToString (Check False) = ""

mateToString :: Mate -> String
mateToString (Mate True) = "#"
mateToString (Mate False) = ""