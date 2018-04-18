module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment

import Board
import Draw
import Input
import AI

-- The main function, which is the starting point of the program.
--
-- The playIO in the main function takes 6 arguments.
-- The first argument of the playIO describes the display window object of the game.
-- The second argument of the playIO is the background colour of the gird.
-- The third argument of the playIO is the number of simulation steps to take for each second of real time.
-- The forth argument of the playIO is the function called "drawWorld" that draws the graphical output on the screen.
-- The fifth argument of the playIO is the function called "handleInput" that handles the mouse event.
-- The sixth argument of the playIO is the "updateWorld" that updates the move that the player and the AI made. (would be players if the user uses the pvp option)
main :: IO ()
main = do args <- getArgs --gets the command line arguments
          hint_piece <- loadBMP "./Assets/hint_piece.bmp" -- reads the "hint_piece.bmp" file.
          black_piece <- loadBMP "./Assets/black_piece.bmp" -- reads the "black_piece.bmp" file.
          white_piece <- loadBMP "./Assets/white_piece.bmp" -- reads the "white_piece.bmp" file.
          undo_button <- loadBMP "./Assets/undo_button.bmp" -- reads the "undo_button.bmp" file.
          save_button <- loadBMP "./Assets/save_button.bmp" -- reads the "save_button.bmp" file.
          load_button <- loadBMP "./Assets/load_button.bmp" -- reads the "load_button.bmp" file.
          hint_button <- loadBMP "./Assets/hint_button.bmp" -- reads the "hint_button.bmp" file.
          black_win <- loadBMP "./Assets/black_win.bmp" -- reads the "black_win.bmp" file.
          white_win <- loadBMP "./Assets/white_win.bmp" -- reads the "white_win.bmp" file.
          if (length(args) /= 5 && length(args) /= 0) --check the number of the command line arguments.
            then print("Usage: ./gomoku <board_size (Int)> <target (Int)> <player (White/Black)> <game_type (normal/4x4)> <ai_level (beginner/intermediate/pvp)>") -- if the user input the wrong number of command line arguments, print out the warning message.
          else if (length(args) == 0) -- if there is no command line argument, draw the option page on the screen
            then playIO (InWindow "Gomoku" (840, 700) (10, 10)) (makeColor 0.7333 0.5569 0.2509 0) 10 (initWorld 6 3 False [] Black White "normal" False "intermediate" (hint_piece:black_piece:white_piece:[]) (undo_button:save_button:load_button:hint_button:[]) (black_win:white_win:[])) drawWorld handleInput updateWorld
          else do --otherwise, start the game.
            let size = read (args!!0) :: Int -- change the type of the "size of the board" from string to Int.
            let target = read (args!!1) :: Int -- change the type of the "target number to win" from string to Int.
            let player = read (args!!2) :: Col -- change the type of the "colour that the player plays with" from string to Col.
            let game_type = (args!!3)
            let ai = (args!!4)
            playIO (InWindow "Gomoku" (840, 700) (10, 10)) (makeColor 0.7333 0.5569 0.2509 0) 10 (initWorld size target False [] Black player game_type True ai (hint_piece:black_piece:white_piece:[]) (undo_button:save_button:load_button:hint_button:[]) (black_win:white_win:[])) drawWorld handleInput updateWorld
          -- initWorld in Board.hs
          -- drawWorld in Draw.hs
          -- handleInput in Input.hs
          -- updateWorld in AI.hs
