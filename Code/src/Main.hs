module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment

import Board
import Draw
import Input
import AI

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move

main :: IO ()
main = do args <- getArgs
          if (length(args) /= 5 && length(args) /= 0)
            then print("Usage: ./gomoku <board_size (Int)> <target (Int)> <player (White/Black)> <game_type (normal/4x4)> <ai_level (beginner/intermediate/pvp)>")
          else if (length(args) == 0)
            then playIO (InWindow "Gomoku" (840, 700) (10, 10)) (greyN 0.4) 10 (initWorld 6 3 [] Black White "normal" False "intermediate") drawWorld handleInput updateWorld
          else do
            let size = read (args!!0) :: Int
            let target = read (args!!1) :: Int
            let player = read (args!!2) :: Col
            let game_type = (args!!3)
            let ai = (args!!4)
            playIO (InWindow "Gomoku" (840, 700) (10, 10)) (greyN 0.4) 10 (initWorld size target [] Black player game_type True ai) drawWorld handleInput updateWorld
          -- initWorld in Board.hs
          -- drawWorld in Draw.hs
          -- handleInput in Input.hs
          -- updateWorld in AI.hs
