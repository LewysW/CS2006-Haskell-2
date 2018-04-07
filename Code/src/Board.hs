module Board where

import Data.List ((\\))
import Debug.Trace
import System.IO
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Data.List.Split
import System.IO.Unsafe


spacing = 50

data Col = Black | White
  deriving (Show, Eq, Read)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
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
initBoard :: Int -> Int -> [(Position, Col)] -> Board
initBoard s c ps = Board s c ps

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).

data World = World { board :: Board,
                     turn :: Col,
                     player :: Col,
                     buttons :: [Button] }


initWorld :: Int -> Int -> [(Position, Col)] -> Col -> IO World
initWorld size target history player = return $ World (initBoard size target history) Black player (allButtons size)


-- List of all buttons that the game uses.
allButtons :: Int -> [Button]
allButtons s = adjustButtons s [undoButton, saveButton, loadButton]

getSize size = (fromIntegral ( ((size - 1) * spacing)) / 2)

adjustButtons :: Int -> [Button] -> [Button]
adjustButtons size = map adjustB
  where adjustB b = b { topLeft = (((fst (topLeft b) - round(getSize size) - 40)), ((snd (topLeft b) + round(getSize size)))),
                       bottomRight = (((fst (bottomRight b) - round(getSize size) - 40)), ((snd (bottomRight b) + round (getSize size))))
                      }

-- A function that returns to the last turn of the current player, thus, it actually undoes 2 turns.
undo :: IO World -> IO World
undo world = do w <- world
                if (checker w)
                   then return w
                   else return $ (w { board = (board w) { pieces = remove (pieces (board w)) 2 } })
  where checker w = checkIfFirstTurn (pieces (board w))


-- A Button that rolls back one turn for the current player
undoButton :: Button
undoButton = Button { topLeft = (-80, 0), bottomRight = (0, -30), value = "Undo Move", action = undo }


-- A button to save the game
saveButton :: Button
saveButton = Button { topLeft = (-80, -40), bottomRight = (0, -70), value = "Save Game", action = save }

save :: IO World -> IO World
save w = do world <- w
            writeFile "save.dat" (show (size (board world)) ++ " " ++ show (target (board world))) --Overwrite the old save file and write the size and target of the current board
            forM_ (pieces (board world)) outputPiece --Write the position and colour of every space on the board
            return world

outputPiece :: (Position, Col) -> IO()
outputPiece (p, c) = do appendFile "save.dat" ("\n" ++ show (fst p) ++ " " ++ show (snd p) ++ " " ++ show c)

-- A button to load the save file
loadButton :: Button
loadButton = Button { topLeft = (-80, -80), bottomRight = (0, -110), value = "Load Game", action = load }

-- |Load the current game state from a file
load :: IO World -> IO World
load w = do world <- w
            initWorld new_size new_target new_ps (player world)
         where f = readFile "save.dat" -- Read in the raw save file data
               ls = splitOn "\n" (unsafePerformIO f) -- Split the save file into lines
               top = splitOn " " (head ls) -- Grab each word from the top line of the save file
               new_size = read (head top) :: Int -- First word of the top line is the saved game's board size
               new_target = read (top!!1) :: Int -- Second word of the top line is the saved game's target
               new_ps = (parseSaveFile (tail ls)) -- Parse the rest of the save file into a list of pieces
               new_turn = other (read (splitOn " " (last ls)!!2) :: Col) -- The third word in the final line is the colour of the player who went last (next turn is other colour)


-- This function parse the saved file to a list of pieces
parseSaveFile :: [String] -> [(Position, Col)]
parseSaveFile [] = []
parseSaveFile ls = (pos, col) : parseSaveFile (tail ls)
           where line = splitOn " " (head ls)
                 x = read (head line) :: Int
                 y = read (line !! 1) :: Int
                 pos = (x, y)
                 col = read (line !! 2) :: Col



--Check if it's the first turn.
checkIfFirstTurn :: [(Position, Col)] -> Bool
checkIfFirstTurn (x:xs) = b
  where b = checkIfOnePieceOnTheBoard xs

--Check if there is only one piece on the board.
checkIfOnePieceOnTheBoard :: [(Position, Col)] -> Bool
checkIfOnePieceOnTheBoard [] = True;
checkIfOnePieceOnTheBoard _ = False;

-- This function removes the n elements from the front of a list.
remove :: [a] -> Int -> [a]
remove [] _ = []
remove xs 0 = xs
remove (x:xs) n = remove xs (n - 1)


-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
-- If the movement makes the piece to go out of bounds of the board, return Nothing.
-- Otherwise, add the new piece on the board.
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = case getPiece (pieces board) position of
  Just x -> Nothing -- Position already occupied
  -- If the position is out of bounds of the board, return Nothing.
  Nothing -> if ((fst position < 0) || (fst position > size board - 1) || (snd position < 0) || (snd position > size board - 1) )
                then Nothing
                else Just board { pieces = (position, colour) : pieces board }


-- Get the current piece at the given position.
-- If there is no corresponding piece, return Nothing.
getPiece :: [(Position, Col)] -> Position -> Maybe (Position, Col)
getPiece [] position = Nothing
getPiece (x:xs) position | fst x == position = Just x
                            | otherwise = getPiece xs position


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board = checkBoardPieces (pieces board) board


{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)
In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- Check all 8 possible directions for the checkWon function.
-- It checks if there is piece on the given direction, by calling the countBoardPieces function.
checkBoardPieces :: [(Position, Col)] -> Board -> Maybe Col
checkBoardPieces [] _ = Nothing
-- Count pieces on the all 8 directions.
checkBoardPieces (x:xs) board | (countBoardPieces x pb (0, 1) sb (snd x)) + (countBoardPieces x pb (0, -1) sb (snd x)) - 1 == tb = Just (snd x) -- North & South
                                   | (countBoardPieces x pb (1, 0) sb (snd x)) + (countBoardPieces x pb (-1, 0) sb (snd x)) - 1 == tb = Just (snd x) -- East & West
                                   | (countBoardPieces x pb (1, 1) sb (snd x)) + (countBoardPieces x pb (-1, -1) sb (snd x)) - 1 == tb = Just (snd x) -- North East & South West
                                   | (countBoardPieces x pb (-1, 1) sb (snd x)) + (countBoardPieces x pb (1, -1) sb (snd x)) - 1 == tb = Just (snd x) -- North West & South East
                                   | otherwise = checkBoardPieces xs board
                                   where pb = pieces board
                                         sb = size board
                                         tb = target board


-- This function takes a piece, direction, size and colour, and count the number of the pieces in that direction.
-- Moreover, it is not allowed to go out of the bounds of the board.
-- Thus, this function should check the position of the piece is in the bound of the board.
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


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate board col = getNumConsecutive board col

getNumConsecutive :: Board -> Col -> Int
getNumConsecutive board col = sum(map (getConsecutive (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)])

getConsecutive :: [(Position, Col)] -> Board -> Col -> (Int, Int) -> Int
getConsecutive [] _ _ _ = 0
getConsecutive (x:xs) board col dir | ((countBoardPieces x (pieces board) dir (size board) col) > 1) = 1 + (getConsecutive xs board col dir)
                                | otherwise = (getConsecutive xs board col dir)


getAverageLengthOfSets :: Board -> Col -> Int
getAverageLengthOfSets board col = undefined

--[(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)]

getNumOpenEndedSets :: Board -> Col -> Int
getNumOpenEndedSets board col = undefined
