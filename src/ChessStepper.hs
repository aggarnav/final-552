module ChessStepper (stepper) where

import ChessGame (initialGame, playMoves, printGame)
import ChessParser (parseFile, parseMoves, singlePretty)
import ChessSyntax
  ( Game,
    Move,
    MoveResult (ContinueGame, InvalidMove, Won),
  )
import Data.List as List (uncons)
import Text.Parsec.Error (errorPos)
import Text.Parsec.Pos (sourceName)

-------------
-- Stepper --
-------------

-- Stepper data type
data Stepper = Stepper
  { game :: Game,
    history :: Maybe Stepper
  }

-- initial stepper
initialStepper :: Stepper
initialStepper =
  Stepper
    { game = initialGame,
      history = Nothing
    }

-- take moves, and print the current setup at each turn
stepper :: IO ()
stepper = do
  putStrLn
    "Welcome to Chess Parser! Enter :u to undo, :q to quit, :f to load\
    \ a file, and :r to restart"
  go initialStepper

-- take a stepper, and play the game
go :: Stepper -> IO ()
go s = do
  putStrLn (printGame (game s))
  putStrLn "Enter a move:"
  input <- getLine
  case List.uncons (words input) of
    Just (":f", [fn]) -> do
      m <- parseFile fn
      case m of
        Left x ->
          if fn == (sourceName . errorPos) x
            then do
              putStrLn "File does not exist."
              go s
            else movesStepper s m
        _ -> movesStepper s m
    Just (":q", _) -> do
      putStrLn "Goodbye!"
    Just (":u", _) -> do
      putStrLn "Undoing..."
      case history s of
        Nothing -> do
          putStrLn "No history to undo"
          go s
        Just h -> do
          go h
    Just (":r", _) -> do
      putStrLn "Restarting..."
      go initialStepper
    Just _ -> do
      movesStepper s (parseMoves input)
    Nothing -> do
      putStrLn "Please enter an input"
      go s

-- take a list of moves, and play them
movesStepper :: Stepper -> Either a [Move] -> IO ()
movesStepper s (Left err) = do
  putStrLn "Invalid move format"
  go s
movesStepper s (Right []) = do
  go s
movesStepper s (Right (m : ms)) =
  let (result, newGame) = playMoves [m] (game s)
   in case result of
        InvalidMove -> do
          putStrLn ("Invalid move: " ++ singlePretty m)
          go s
        Won c -> do
          putStrLn (show c ++ " won!")
          go initialStepper
        ContinueGame -> do
          let newStepper = Stepper {game = newGame, history = Just s}
          movesStepper newStepper (Right ms) -- allow for multiple move undos