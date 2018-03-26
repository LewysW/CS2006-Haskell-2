module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
    = case checkWon (board w) of --check if the game is over
           Just c  -> w --if so, ignore the input.
           Nothing -> case getPosition (0, 0) (floor x, floor y) (size (board w)) of --if not, get the clicked position.
                           Just pos -> case makeMove (board w) (turn w) pos of -- If it is an actual position, make a move.
                                            Just b  -> w { board = b, turn = other (turn w) } -- Set the new board and turn
                                            Nothing -> w -- Or make no change.
                           -- Otherwise there is nothing there, so ignore it
                           Nothing  -> w
--handleInput (EventMotion (x, y)) b = trace ("Mouse moved to: " ++ show (x,y)) b
--handleInput (EventKey (Char k) Down _ _) b = trace ("Key " ++ show k ++ " down") b
--handleInput (EventKey (Char k) Up _ _) b = trace ("Key " ++ show k ++ " up") b
handleInput e b = b

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

spacing = 25

-- Check all possible points on the grid and compare to converted coordinates
getPosition :: Position -> Position -> Int -> Maybe Position
getPosition (xo, yo) (xm, ym) size
  | xo == size && yo == size = isPos (xo, yo) (xm, ym) size -- Last corner, which is the border of the board, so check and return
  | yo == size = case isPos (xo, yo) (xm, ym) size of -- End of row, so move to the next row.
                      Just p  -> Just p
                      Nothing -> getPosition (xo + 1, 0) (xm, ym) size
  | otherwise = case isPos (xo, yo) (xm, ym) size of -- All other points.
                    Just p  -> Just p
                    Nothing -> getPosition (xo, yo + 1) (xm, ym) size

isPos :: Position -> Position -> Int -> Maybe Position
isPos (xo, yo) (xm, ym) size
  -- Check whether mouse is approximately at playable point
  | convert xo size >= xm - 10 && convert xo size <= xm + 10 && convert yo size >= ym - 10 && convert yo size <= ym + 10 = Just (xo, yo)
  | otherwise = Nothing
  where convert a size = a * spacing - halfsize size -- Convert board coordinates to pixel coordinates
        halfsize size = ((size - 1) * spacing) `div` 2
