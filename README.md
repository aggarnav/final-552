# Haskess (Haskell Chess) 

Names: Arnav Aggarwal (arnavagg) and Huanming Song (noeland)
Advisor: Cassia Torzcon
Class: CIS 5520: Advanced Programming (Fall 2023, Prof. Weirich)

This is a fully-functioning implementation of Chess. You can play against a person locally or load past games from files. The game supports the official algebraic notation for moves (including disambiguation and resigns) as mentioned at https://en.wikipedia.org/wiki/Algebraic_notation_(chess). You can also undo individual moves, including when you load incomplete games. Sample games in algebraic notation from a range of experience levels are provided in `test/games` from https://www.chessgames.com/ 


## Module organization

We have the following four modules:

  - `ChessSyntax.hs` Contains the data types that we defined to be specific to chess. Listing data types created abstraction for us to work on the game functionality and the parser synchronously.
  
  - `ChessParser.hs` Parses inputs according to the Chess algebraic notation
  
  - `ChessGame.hs` Processes moves and keeps track of the current game using the State Monad. The file contains the logic for the rules of the game.

  - `ChessStepper.hs` Implements our command line interface to query the user for an input, parse it and, and run it using ChessGame.

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 