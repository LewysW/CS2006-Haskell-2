module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

-- handles events and returns the world that is updated based on the event
-- @Event - The event to be handled, @(IO World) - the world to be updated, @(IO(IO World)) - the updated world that is returned
handleInput :: Event -> IO World -> IO (IO World)
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) world =
  do w <- world
     case (running w) of -- if game is not running, only check for loading screen button clicks
       True -> case checkWon (board w) of -- Check if the game has been finished
                    Just c  -> (handleButtons world (x, y)) -- Player won, check for button presses but not moves
                    Nothing -> case getPosition (0, 0) (floor x, floor y) (size (board w)) of -- Try getting the position on the board of the point clicked
                                    Just pos -> case ((game_type w) == "4x4" && (checkFourAndFour (board w) (turn w) pos)) of -- Special case checks for 4x4 rule variant
                                                     True -> (handleButtons world (x, y)) -- Do not allow making a move
                                                     otherwise -> case makeMove (board w) (turn w) pos of -- If it is an actual position, make a move
                                                                      Just b  -> (handleButtons (nextTurn w b) (x, y)) -- Set the new board and turn and check buttons for clicking
                                                                      Nothing -> (handleButtons world (x, y))-- Or make no change and check buttons for clicking
                                    Nothing  ->  handleButtons world (x, y) -- Otherwise there is nothing there, so ignore it and check the buttons for clicking
       False -> ((handleButtons world (x, y))) -- Check for just button clicks if game is in the loading screen phase
  where nextTurn w b = return (w { board = b, turn = other (turn w), isUpdated = False }) -- Update the turn
        handleButtons w (x, y) = do world <- w
                                    return (checkButtons w (floor x, floor y) (buttons world)) -- Check for button clicks

-- Return the world for any event that is not a left mouse click
handleInput e w = return w

-- Checks each button individually to see whether it has been clicked and performs the required action
-- @(IO World) - The world that contains information about buttons and is to be updated, @Position - The mouse click coordinates, @[button] - list of buttons to be checked, @(IO World) - return the updated world
checkButtons :: IO World -> Position -> [Button] -> IO World
checkButtons w _ [] = w -- Base case - no button click happened

-- Checks if the click was made on the button's position
checkButtons w (xm, ym) (b:bs) = if xm > fst (topLeft b) && xm < fst (bottomRight b) && ym < snd (topLeft b) && ym > snd (bottomRight b) -- Click needs to be within the button's coordinate boundaries
                                    then (action b w) -- Run the buttons's function and return
                                 else (checkButtons w (xm, ym) bs) -- Otherwise check the rest of the buttons

-- Checks all possible points on the grid and compare to converted coordinates
-- @Position - coordinates of the grid, @Position - mouse coordinates, @Int - size of the board, @(Maybe Position) - returns a position if click happened on valid grid spot
getPosition :: Position -> Position -> Int -> Maybe Position

getPosition (xo, yo) (xm, ym) size
  -- Last corner of grid, so check and return
  | xo == size && yo == size = isPos (xo, yo) (xm, ym) size
  -- End of row, so move up to next row
  | yo == size = case isPos (xo, yo) (xm, ym) size of -- convert coordinates and check result
                     Just p  -> Just p
                     Nothing -> getPosition (xo + 1, 0) (xm, ym) size
  -- Just move along the row, incrementing the column index
  | otherwise = case isPos (xo, yo) (xm, ym) size of
                    Just p  -> Just p
                    Nothing -> getPosition (xo, yo + 1) (xm, ym) size

-- Checks whether mouse is approximately at playable point and returns converted coordinates
-- @Position - coordinates of the grid, @Position - mouse coordinates, @Int - size of the board, @(Maybe Position) - returns a converted position coordinate if click is valid
isPos :: Position -> Position -> Int -> Maybe Position
isPos (xo, yo) (xm, ym) size
  -- Checks for approximate location
  | convert xo size >= xm - pad && convert xo size <= xm + pad &&
    convert yo size >= ym - pad && convert yo size <= ym + pad
    = Just (xo, yo)
  | otherwise = Nothing
  -- Convert board coordinates to pixel coordinates
  where pad = 10 -- padding (pixels) around each point
        convert a size = a * spacing - halfsize size
        halfsize size = ((size - 1) * spacing) `div` 2
