module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
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
handleInput :: Event -> IO World -> IO (IO World)
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world =
  do w <- world
    -- If the AI is playing, do not accept input
     case checkWon (board w) of -- Check if the game has been finished
          Just c  -> return $ handleButtons world (x, y) -- Player won, check for button presses but not moves
          Nothing -> case getPosition (0, 0) (floor x, floor y) (size (board w)) of -- Try getting the position on the board of the point clicked
                          Just pos -> case ((game_type w) == "4x4" && (checkFourAndFour (board w) (turn w) pos)) of
                                           True -> return $ handleButtons world (x, y)
                                           otherwise -> case makeMove (board w) (turn w) pos of -- If it is an actual position, make a move
                                                            Just b  -> return $ handleButtons (nextTurn w b) (x, y) -- Set the new board and turn and check buttons for clicking
                                                            Nothing -> return $ handleButtons world (x, y)-- Or make no change and check buttons for clicking
                          Nothing  -> return $ handleButtons world (x, y) -- Otherwise there is nothing there, so ignore it and check the buttons for clicking
  where nextTurn w b = return w { board = b, turn = other (turn w) }
        handleButtons w (x, y) = do world <- w
                                    checkButtons w (floor x, floor y) (buttons world)


handleInput e b = return b


checkButtons :: IO World -> Position -> [Button] -> IO World
checkButtons w _ [] = w
-- Where w is the world, xm;ym are mouse coordinates, b is button, bs is list
-- If the click was made on the button's position
checkButtons w (xm, ym) (b:bs) = if xm > fst (topLeft b) && xm < fst (bottomRight b) && ym < snd (topLeft b) && ym > snd (bottomRight b)
                                    then action b w -- Run the buttons's function and return
                                    else checkButtons w (xm, ym) bs -- Otherwise check the rest of the buttons

getPosition :: Position -> Position -> Int -> Maybe Position
-- Check all possible points on the grid and compare to converted coordinates
getPosition (xo, yo) (xm, ym) size -- (xo;yo coordinates of grid, xm;ym mouse coordinates)
  -- Last corner of grid, so check and return
  | xo == size && yo == size = isPos (xo, yo) (xm, ym) size
  -- End of row, so move up to next row
  | yo == size = case isPos (xo, yo) (xm, ym) size of
                     Just p  -> Just p
                     Nothing -> getPosition (xo + 1, 0) (xm, ym) size
  -- Just move along the row, incrementing the column index
  | otherwise = case isPos (xo, yo) (xm, ym) size of
                    Just p  -> Just p
                    Nothing -> getPosition (xo, yo + 1) (xm, ym) size

isPos :: Position -> Position -> Int -> Maybe Position
isPos (xo, yo) (xm, ym) size
  -- Check whether mouse is approximately at playable point
  | convert xo size >= xm - pad && convert xo size <= xm + pad &&
    convert yo size >= ym - pad && convert yo size <= ym + pad
    = Just (xo, yo)
  | otherwise = Nothing
  -- Convert board coordinates to pixel coordinates
  where pad = 10 -- padding (pixels) around each point
        convert a size = a * spacing - halfsize size
        halfsize size = ((size - 1) * spacing) `div` 2
