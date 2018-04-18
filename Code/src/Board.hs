module Board where

import Data.List ((\\))
import Debug.Trace
import System.IO
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Data.List.Split
import Data.Maybe
import System.IO.Unsafe
import Graphics.Gloss


spacing = 50

data Col = Black | White
  deriving (Show, Eq, Read)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)
type Hint = Bool

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)],
                     hint :: Position,
                     hints :: Bool
                   }
  deriving Show

-- A Button contains it's top-left and bottom-right coordinates,
-- the value that it displays and a function that changes the
-- world state.
data Button = Button { topLeft :: Position,
                       bottomRight :: Position,
                       value :: String,
                       action :: IO World -> IO World
                     }

-- Default board is 6x6, target is 3 in a row, no initial pieces

--Initialize the board.
-- @Int - size dimension of board, @Int - target to win game,
-- @[(Position, Col)] - pieces on board, @Position - location of hint piece,
-- @Bool - whether or not hints are enabled, @Board - initialised board
initBoard :: Int -> Int -> [(Position, Col)] -> Position -> Bool -> Board
initBoard s t ps pos hints = Board s t ps pos hints

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).

data World = World { board :: Board,
                     turn :: Col,
                     isUpdated :: Bool,
                     player :: Col,
                     game_type :: String,
                     buttons :: [Button],
                     running :: Bool,
                     ai:: String,
                     game_images :: [Picture],
                     button_images :: [Picture],
                     winscreen_images :: [Picture]}

-- Initialises the world use to store the state of the program
-- @Int - size of board, @Int - target goal to win, @Bool - whether board has been updated,
-- @[(Position, Col)] - state of board, @Col - whose turn it is, @Col - colour of player,
-- @String - game mode, @Bool - whether game or load screen is running, @String - type of AI,
-- @[Picture] - BMP game images, @[Picture] - BMP button images, @[Picture] - BMP win screen images
-- @IOWorld - world to store state of program
initWorld :: Int -> Int -> Bool -> [(Position, Col)] -> Col -> Col -> String -> Bool -> String -> [Picture] -> [Picture] -> [Picture] -> IO World
initWorld size target isUpdated history turn player game_type running ai gImages bImages wImages = if running -- set default hint position as the middle piece TODO
                                                                                                    then return $ World (initBoard size target history ((calcPos size), (calcPos size)) False) turn isUpdated player game_type (gameButtons size) running ai gImages bImages wImages
                                                                                                   else return $ World (initBoard size target history ((calcPos size), (calcPos size)) False) turn isUpdated player game_type (loadingButtons size) running ai gImages bImages wImages
  where calcPos size = (size `div` 2)

-- Sets positions of buttons displayed while user is running game (adjusts position)
-- based on size of board.
-- @Int - size of board, @[Button] - list of game buttons
gameButtons :: Int -> [Button]
gameButtons s = adjustButtons s [undoButton, saveButton, loadButton, hintsButton]

-- Sets positions of buttons displayed during the loading page of the game when
-- the user has the opportunity to change the game settings.
-- @Int - size of board, @[Button] - list of loading screen buttons.
loadingButtons :: Int -> [Button]
loadingButtons s = adjustButtons s [sizeSet5Button, sizeSet10Button, sizeSet19Button, targetSet3Button, targetSet5Button, targetSet7Button, whiteButton, blackButton, aiBeginnerButton, aiIntermediateButton, aiPVPButton, normalGameButton, fourAndFourGameButton]

--Gets the spacing required to format the buttons nicely.
-- @Int - size dimension of board, @Float - amount to adjust buttons by
getSize :: Int -> Float
getSize size = (fromIntegral ( ((size - 1) * spacing)) / 2)

-- Adjusts button positions using value returned from getSize to make buttons
-- display nicely.
-- @Int - size dimension of board, @[Button] - lists of buttons to adjust positions of,
-- @[Button] - list of buttons with adjusted positions.
adjustButtons :: Int -> [Button] -> [Button]
adjustButtons size = map adjustB
  where adjustB b = b { topLeft = (((fst (topLeft b) - round(getSize size)) - 40), ((snd (topLeft b) + round(getSize size)))),
                       bottomRight = (((fst (bottomRight b) - round(getSize size)) - 40), ((snd (bottomRight b) + round (getSize size))))
                      }

-- A function that returns to the last turn of the current player, thus, it actually undoes 2 turns.
-- @Int - number of turns to undo, @IO World - world to revert to previous turn.
-- @IO World - world reverted to previous board state.
undo :: Int -> IO World -> IO World
undo a world = do w <- world
                  if (checker w || (checkWon (board w) /= Nothing))
                     then return w
                     else return $ (w { board = (board w) { pieces = remove (pieces (board w)) 2 } })
    where checker w = checkIfFirstTurn (pieces (board w))

-- Sets the position, text value and action of each of the buttons to select the size of the board on the loading screen.
sizeSet5Button :: Button
sizeSet5Button = Button { topLeft = (-80, 130), bottomRight = (70, 100), value = "Set Size 6", action = (setSize 6) }
sizeSet10Button :: Button
sizeSet10Button = Button { topLeft = (80, 130), bottomRight = (230, 100), value = "Set Size 10", action = (setSize 10) }
sizeSet19Button :: Button
sizeSet19Button = Button { topLeft = (240, 130), bottomRight = (390, 100), value = "Set Size 19", action = (setSize 19) }

-- Sets the position, text value and action of each of the buttons to select the target of the board on the loading screen.
targetSet3Button :: Button
targetSet3Button = Button { topLeft = (-80, 90), bottomRight = (70, 60), value = "Set Target 3", action = (setTarget 3) }
targetSet5Button :: Button
targetSet5Button = Button { topLeft = (80, 90), bottomRight = (230, 60), value = "Set Target 5", action = (setTarget 5) }
targetSet7Button :: Button
targetSet7Button = Button { topLeft = (240, 90), bottomRight = (390, 60), value = "Set Target 7", action = (setTarget 7) }

-- Sets the position, text value and action of each of the buttons to select which colour the player will play as.
whiteButton :: Button
whiteButton = Button { topLeft = (-80, 50), bottomRight = (70, 20), value = "Play as White", action = (setWhite 0) }
blackButton :: Button
blackButton = Button { topLeft = (80, 50), bottomRight = (230, 20), value = "Play as Black", action = (setBlack 0) }

-- Sets the position, text value and action of each of the buttons to select which AI mode to play with
aiBeginnerButton :: Button
aiBeginnerButton = Button { topLeft = (-80, 10), bottomRight = (70, -20), value = "Beginner AI", action = (setAI "beginner") }
aiIntermediateButton :: Button
aiIntermediateButton = Button { topLeft = (80, 10), bottomRight = (230, -20), value = "Intermediate AI", action = (setAI "intermediate") }
aiPVPButton :: Button
aiPVPButton = Button { topLeft = (240, 10), bottomRight = (390, -20), value = "PVP", action = (setAI "pvp") }

-- Sets the position, text value and action of each of the buttons to select which game mode to play.
normalGameButton :: Button
normalGameButton = Button { topLeft = (-80, -30), bottomRight = (70, -60), value = "Start Normal Game", action = (initNormal 0) }
fourAndFourGameButton :: Button
fourAndFourGameButton = Button { topLeft = (80, -30), bottomRight = (230, -60), value = "Start 4x4 Game", action = (initFourAndFour 0) }

--Sets the size dimension of the board.
-- @Int - size to set board to, @IO World - world containing board
-- @IO World - world with updated board size dimension.
setSize :: Int -> IO World -> IO World
setSize a w = do world <- w
                 (return $ world { board = (board world) { size = a } })

--Sets the target (win condition) of the board.
-- @Int - target (i.e. number in a row to win), @IO World - world containing board.
-- @IO World - updated world with updated target.
setTarget :: Int -> IO World -> IO World
setTarget a w = do world <- w
                   (return $ world { board = (board world) { target = a } })

--Sets the player to be the colour white.
-- @IO World - world to update player colour
-- @IO World - updated world with new player colour.
setWhite :: Int -> IO World -> IO World
setWhite _ w = do world <- w
                  (return $ world { player = White })

--Sets the player to be the colour Black.
-- @IO World - world to update player colour
-- @IO World - updated world with new player colour.
setBlack :: Int -> IO World -> IO World
setBlack _ w = do world <- w
                  (return $ world { player = Black })

--Sets the AI mode (Beginner - i.e. random AI, Intermediate - i.e. heuristic AI, or
-- PVP - player vs player)
-- @String - AI mode, @IO World - world to be updated with AI mode.
-- @IO World - updated world with AI mode.
setAI :: String -> IO World -> IO World
setAI level w = do world <- w
                   (return $ world { ai = level })

-- Initialises the game mode to be normal (as opposed to the rule of four and four)
-- @IO World - world to have game mode set. @IO World - updated game world with new game mode.
initNormal :: Int -> IO World -> IO World
initNormal _ w = do world <- w
                    (initWorld (size (board world)) (target (board world)) (isUpdated world) [] Black (player world) ("normal") True (ai world) (game_images world) (button_images world) (winscreen_images world))

-- Initialises the game mode to be the rule of four and four (as opposed to the normal game mode)
-- @IO World - world to have game mode set. @IO World - updated game world with new game mode.
initFourAndFour :: Int -> IO World -> IO World
initFourAndFour _ w = do world <- w
                         (initWorld (size (board world)) (target (board world)) (isUpdated world) [] Black (player world) ("4x4") True (ai world) (game_images world) (button_images world) (winscreen_images world))

-- Sets the position, text value and action of the undo button to roll back the game turn and state.
undoButton :: Button
undoButton = Button { topLeft = (-150, 0), bottomRight = (-70, -30), value = "Undo Move", action = (undo 0) }

-- Sets the position, text value and action of each of the button to toggle hints on or off.
hintsButton :: Button
hintsButton = Button { topLeft = (-150, -120), bottomRight = (-70, -150), value = "Toggle Hints", action = (toggleHints) }

-- When the hintsButton is clicked, toggles the value of hints in the world to turn
-- hints on or off.
-- @IO World - world to toggle hints on or off.
-- @IO World - world with new value of hints.
toggleHints :: IO World -> IO World
toggleHints w = do world <- w
                   let hintsVar = (hints (board world))
                   (return $ world { board = (board world) { hints = not hintsVar } })

-- Sets the position, text value and action of the save button which saves the state of the game.
saveButton :: Button
saveButton = Button { topLeft = (-150, -40), bottomRight = (-70, -70), value = "Save Game", action = save }

-- Called when the save button is clicked. Writes the current size of the board,
-- board target, and game type to the save.dat file. In addition, saves the positions
-- of all of the pieces (and associated colours) to the save.dat file.
-- @IO World - world with settings to be saved to file.
-- @IO World - world with saved settings.
save :: IO World -> IO World
save w = do world <- w
            writeFile "save.dat" (show (size (board world)) ++ " " ++ show (target (board world)) ++ " " ++ (game_type world)) --Overwrite the old save file and write the size and target of the current board
            forM_ (pieces (board world)) outputPiece --Write the position and colour of every space on the board
            return world

-- Outputs the current piece to the save file.
-- @(Position, Col) - current piece to save
-- @IO() - IO enabling writing to file.
outputPiece :: (Position, Col) -> IO()
outputPiece (p, c) = do appendFile "save.dat" ("\n" ++ show (fst p) ++ " " ++ show (snd p) ++ " " ++ show c)

-- Sets the position, text value and action of each of the button to load in the save file.
loadButton :: Button
loadButton = Button { topLeft = (-150, -80), bottomRight = (-70, -110), value = "Load Game", action = load }

-- Called when the load button is clicked. Initialises the world with the data from the save.dat file.
-- @IO World - world to load save file data into.
-- @IO World - new world initialised with save file data.
load :: IO World -> IO World
load w = do world <- w
            initWorld new_size new_target False new_ps (turn world) (player world) new_game_type True (ai world) (game_images world) (button_images world) (winscreen_images world)
         where f = readFile "save.dat" -- Read in the raw save file data
               ls = splitOn "\n" (unsafePerformIO f) -- Split the save file into lines
               top = splitOn " " (head ls) -- Grab each word from the top line of the save file
               new_size = read (head top) :: Int -- First word of the top line is the saved game's board size
               new_target = read (top!!1) :: Int -- Second word of the top line is the saved game's target
               new_game_type = (top!!2)
               new_ps = (parseSaveFile (tail ls)) -- Parse the rest of the save file into a list of pieces
               new_turn = other (read (splitOn " " (last ls)!!2) :: Col) -- The third word in the final line is the colour of the player who went last (next turn is other colour)


-- Parses the position strings from file into a list of pieces.
-- @[String] - position strings from file, @[(Position, Col)] - parsed pieces
parseSaveFile :: [String] -> [(Position, Col)]
parseSaveFile [] = []
parseSaveFile ls = (pos, col) : parseSaveFile (tail ls)
           where line = splitOn " " (head ls)
                 x = read (head line) :: Int
                 y = read (line !! 1) :: Int
                 pos = (x, y)
                 col = read (line !! 2) :: Col



--Check if it's the first turn of the game.
-- @[(Position, Col)] - list of board pieces, @Bool - whether it's the first turn of game
checkIfFirstTurn :: [(Position, Col)] -> Bool
checkIfFirstTurn [] = False
checkIfFirstTurn (x:xs) = b
  where b = checkIfOnePieceOnTheBoard xs

--Check if there is only one piece on the board.
-- @[(Position, Col)] - list of pieces on board. @Bool - whether there is one piece on board.
checkIfOnePieceOnTheBoard :: [(Position, Col)] -> Bool
checkIfOnePieceOnTheBoard [] = True;
checkIfOnePieceOnTheBoard _ = False;

-- This function removes the n elements from the front of a list.
-- @[a] - list of elements, @Int - n elements to remove,
-- @[a] - list with n elements removed from front of list.
remove :: [a] -> Int -> [a]
remove [] _ = []
remove xs 0 = xs
remove (x:xs) n = remove xs (n - 1)


-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
-- If the movement makes the piece to go out of bounds of the board, return Nothing.
-- Otherwise, add the new piece on the board.
-- @Board - to play move on. @Col - whose turn it is. @Position - where to play piece.
-- @Maybe Board - board with piece placed or Nothing if piece is already there or out of bounds.
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = case getPiece (pieces board) position of
  Just x -> Nothing -- Position already occupied
  -- If the position is out of bounds of the board, return Nothing.
  Nothing -> if ((fst position < 0) || (fst position > size board - 1) || (snd position < 0) || (snd position > size board - 1) )
                then Nothing
                else Just board { pieces = (position, colour) : pieces board }


-- Get the current piece at the given position.
-- If there is no corresponding piece, return Nothing.
-- @[(Position, Col)] - list of pieces. @Position - position to get piece from.
-- Maybe (Position, Col) - piece at position if it exists, or Nothing if there is not piece there
getPiece :: [(Position, Col)] -> Position -> Maybe (Position, Col)
getPiece [] position = Nothing
getPiece (x:xs) position | fst x == position = Just x
                            | otherwise = getPiece xs position


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
-- @Board - board to check for win,
-- @Maybe Col - colour of winner if there is one, or Nothing if no-one has won.
checkWon :: Board -> Maybe Col
checkWon board = checkBoardPieces (pieces board) board

-- Check all 8 possible directions for the checkWon function.
-- It checks if there is piece on the given direction, by calling the countBoardPieces function.
-- @[(Position, Col)] - list of pieces to check, @Board - board to check pieces of.
-- @Maybe Col - colour of player at position, or Nothing if no pieces.
checkBoardPieces :: [(Position, Col)] -> Board -> Maybe Col
checkBoardPieces [] _ = Nothing
-- Count pieces on the all 8 directions.
checkBoardPieces (x:xs) board | (countBoardPieces x pb (0, 1) sb (snd x)) + (countBoardPieces x pb (0, -1) sb (snd x)) - 1 == tb = Just (snd x) -- North & South
                                   | (countBoardPieces x pb (1, 0) sb (snd x)) + (countBoardPieces x pb (-1, 0) sb (snd x)) - 1 == tb = Just (snd x) -- East & West
                                   | (countBoardPieces x pb (1, 1) sb (snd x)) + (countBoardPieces x pb (-1, -1) sb (snd x)) - 1 == tb = Just (snd x) -- North East & South West
                                   | (countBoardPieces x pb (-1, 1) sb (snd x)) + (countBoardPieces x pb (1, -1) sb (snd x)) - 1 == tb = Just (snd x) -- North West & South East
                                   | otherwise = checkBoardPieces [] board
                                   where pb = pieces board
                                         sb = size board
                                         tb = target board


-- Counts the number of the pieces in a given direction.
-- Moreover, it is not allowed to go out of the bounds of the board.
-- Thus, this function should check the position of the piece is in the bound of the board.
-- @(Position, Col) - piece to count from, @[(Position, Col)] - list of pieces on board,
-- @(Int, Int) - Direction to count in, @Int - size of board, @Col - colour pieces to count.
-- @Int - number of pieces of a given colour in a given direction.
countBoardPieces :: (Position, Col) -> [(Position, Col)] -> (Int, Int) -> Int -> Col -> Int
--if the colour of the piece is not matched with the given colour, return 0.
countBoardPieces (_, Black) _ _ _ White = 0
countBoardPieces (_, White) _ _ _ Black = 0
--If the colour matches, count the number of pieces on the board.
countBoardPieces x xs d size colour | (fst (fst x) < 0 || fst (fst x) >= size || snd (fst x) < 0 || snd (fst x) >= size) = 0
                                         -- If the position of the piece is out of bounds of the board, return 0. Otherwise, count the number of pieces.
                                         | otherwise = case getPiece xs (fst (fst x) + fst d, snd (fst x) + snd d) of
                                                            --gets the piece on the given direction by calling the getThePiece function.
                                                            Just piece -> 1 + (countBoardPieces piece xs d size colour)
                                                            Nothing -> 1


-- An evaluation function for board. Returns a float indicating how good the board
-- is for the given player (colour) by calculating the board score for that player and subtracting
-- the score of their opponent from their score.
-- @Board - board to evaluate. @Col - colour of player to get score.
-- @Float - score indicating how good board is.
evaluate :: Board -> Col -> Float
evaluate board col = (getScore board col) - (getScore board (other col))

-- Determines the player's score by using a heuristic determined by the player's
-- consecutive sets of pieces in a row, the average length of those sets,
-- the maximum length of those sets, and the number of sets which cannot result
-- in a win (i.e. have been blocked or closed by the other player). Uses a slightly
-- different heuristic for a target of 3 and targets greater than 3.
-- @Board - board to use to determine score. @Col - Player to get score of.
-- @Float - score for given player.
getScore :: Board -> Col -> Float
getScore board col = if ((target board) == 3)
                        then (realToFrac(getNumConsecutive board col)
                             + 17.5 * (getAverageLength board col)
                             + (realToFrac(maxLength board col))
                             - (4 * realToFrac(getNumClosed board col)))
                        else
                             (realToFrac(getNumConsecutive board col)
                             + 16 * (getAverageLength board col)
                             + (realToFrac(maxLength board col))
                             - (5 * realToFrac(getNumClosed board col)))

-- Gets a list of the maximum length sets of each position in any direction and then
-- gets the maximum of that list. If the maximum length set on the board for the given
-- player is equal to the target of the board, or is equal to one less than the target,
-- then a value of 5000, and 2000 are returned respectively to overwhelm the heuristic as
-- this indicates that the board is close to a win state.
-- @Board - to scan for maximum length set. @Col - colour of pieces to count.
-- @Int - value to be used in heuristic.
maxLength :: Board -> Col -> Int
maxLength board col = let maximumLength = maximum (map maximum (map (getLengths (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)]))
                      in
                        if maximumLength == (target board) then 5000
                        else if maximumLength == ((target board) - 1) then 2000
                        else maximumLength



--Gets number of consecutive sets on the board (sets with a length greater than 1)
-- @Board - board to scan for consecutive sets, @Col - colour of pieces to count.
-- @Int - number of consecutive sets
getNumConsecutive :: Board -> Col -> Int
getNumConsecutive board col = sum(map (getConsecutive (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)])

--Gets the number of closed off sets on the board (i.e. sets that are blocked by
-- the opposing piece in a direction)
-- @Board - board to scan for closed sets, @Col - colour of pieces to check.
-- @Int - number of closed sets.
getNumClosed :: Board -> Col -> Int
getNumClosed board col = length (filter (== True) (getClosed board col (pieces board)))

--Gets the average length of a set on the board if there are any consecutive sets (otherwise returns 0).
-- Does this by getting the lengths of sets in a direction from a single piece, summing them, and then doing
-- this for all pieces on the board, and then divides the total length of sets by the number of consecutive sets.
-- @Board - board to scan for set lengths, @Col - colour to scan for.
-- @Float - average length of sets.
getAverageLength :: Board -> Col -> Float
getAverageLength board col = if (getNumConsecutive board col) > 0
                             then realToFrac (sum(map sum (map (getLengths (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)])))
                                    / realToFrac (getNumConsecutive board col)
                             else 0.0

--Gets the consecutive sets on the board for a player. If there are no pieces on the
-- board then 0 is returned.
-- @[(Position, Col)] - pieces on board, @Board - board to be scanned, @Col - colour to find sets of,
-- @(Int, Int) - direction to check, @Int - number of consecutive in one direction.
getConsecutive :: [(Position, Col)] -> Board -> Col -> (Int, Int) -> Int
getConsecutive [] _ _ _ = 0
getConsecutive (x:xs) board col dir | ((countBoardPieces x (pieces board) dir (size board) col) > 1) = 1 + (getConsecutive xs board col dir)
                                | otherwise = (getConsecutive xs board col dir)

--Gets the lengths of sets on the board that are greater than 1 by counting all sets on
-- the board in a given direction.
-- @[(Position, Col)], @Board - board to scan for set lengths, @Col - colour of pieces to count,
-- @(Int, Int) - direction to count in, @[Int] - list of lengths of sets.
getLengths :: [(Position, Col)] -> Board -> Col -> (Int, Int) -> [Int]
getLengths [] _ _ _ = [0]
getLengths (x:xs) board col dir | ((countBoardPieces x (pieces board) dir (size board) col) > 1) = (countBoardPieces x (pieces board) dir (size board) col) : (getLengths xs board col dir)
                                   | otherwise = (getLengths xs board col dir)


-- Gets the sets on the board that are 'closed' (i.e. sets with a length greater than 1 that are blocked by the other player's piece)
-- Does this by constructing a list of booleans indicating whether a set is closed (true) or not closed (false)
-- @Board - board to be scanned for closed sets, @Col - colour of sets to check if closed,
-- @[(Position, Col)] - positions on board, @[Bool] - list indicating whether a set is closed or not.
getClosed :: Board -> Col -> [(Position, Col)] -> [Bool]
getClosed board col [] = [False]
getClosed board col (x:xs) = (map (isClosed board col x) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)]) ++ (getClosed board col xs)

--Checks whether a set is 'closed'. Closed if getSubsequentPiece does not return Nothing
-- @Board - board to be scanned, @Col - colour to check for, @(Position, Col) - position to check.
-- @Bool - true or false indicating if set is closed.
isClosed :: Board -> Col -> (Position, Col) -> (Int, Int) -> Bool
isClosed board col piece dir = if ((getSubsequentPiece board col piece dir) == Nothing) then False
                                  else True

--Gets the piece after the end of the player's set. Does this by converting the length
-- of the set into a coordinate to add to the starting coordinate of the set which will
-- return the coordinate of the piece at the end of the set. If this piece is Nothing
-- then there is no piece blocking it, otherwise there is a piece blocking it.
-- @Board - board to scan, @Col - colour to check sets of, @(Position, Col) - position to check,
-- @(Int, Int) - direction to check, @Maybe (Position, Col) - Opposing player's piece at end of set,
-- or Nothing if no other piece at end of set.
getSubsequentPiece :: Board -> Col -> (Position, Col) -> (Int, Int) -> Maybe (Position, Col)
getSubsequentPiece board col piece dir = if (countBoardPieces piece (pieces board) dir (size board) col) > 1
                                            then (getPiece (pieces board) (addT (mulT ((countBoardPieces piece (pieces board) dir (size board) col)) dir) (fst (piece))))
                                         else Nothing

--Multiplies a number by a tuple (e.g. a * (b, c) returns (ab, ac))
mulT :: Int -> (Int, Int) -> (Int, Int)
mulT x y = (x * (fst y), x * (snd y))

--Adds two tuples together (e.g. (a, b) + (c, d) returns (a + c, b + d))
addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT x y = ((fst x) + (fst y), (snd x) + (snd y))

-- Checks if the move is legal according to the four by four rule
-- @Board - Board to check for rule, @Col - Colour to check for, @Position - The positition that is attempted to be played, @Bool - Return true if move is illegal, otherwise return false
checkFourAndFour :: Board -> Col -> Position -> Bool
checkFourAndFour board col pos | trace(show(countOpenAndClosed board col pos 4)) (countOpenAndClosed board col pos 4) >= 5 = True
                               | otherwise = False

-- Checks how many individual target in a row cases there are, which is similar to counting the number of open/closed rows
-- @Board - The board to be checked, @Col - the colour to check for, @Position - The positition that is attempted to be played, @Int - The target to check for, @Int - return the individual number of target rows
countOpenAndClosed :: Board -> Col -> Position -> Int -> Int
countOpenAndClosed board col pos target = checkAllBoardPieces ((pos, col) : (pieces board)) ((pos, col) : (pieces board)) board target col -- Update the board temporarily with the position that is attempted to be played

-- Checks how many individual target in a row cases there are
-- @([(Position, Col)]) - All board pieces, @([(Position, Col)]) - All board pieces, @Board - The board to be checked for, @Int - The target in a row to check for, @Col - The color to check for, @Int - return the amount of rows, counted by each individual piece
checkAllBoardPieces :: [(Position, Col)] -> [(Position, Col)] -> Board -> Int -> Col -> Int
checkAllBoardPieces [] _ _ _ _ = 0 -- Base case
-- Count pieces on the all 8 directions.
checkAllBoardPieces (x:xs) pb board tb color | (countBoardPieces x pb (0, 1) sb color) + (countBoardPieces x pb (0, -1) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- North & South - Increment result by one every time a case is found
                                          | (countBoardPieces x pb (1, 0) sb color) + (countBoardPieces x pb (-1, 0) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- East & West
                                          | (countBoardPieces x pb (1, 1) sb color) + (countBoardPieces x pb (-1, -1) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- North East & South West
                                          | (countBoardPieces x pb (-1, 1) sb color) + (countBoardPieces x pb (1, -1) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- North West & South East
                                          | otherwise = 0 + checkAllBoardPieces xs pb board tb color -- No target found, keep same result
                                          where sb = size board
