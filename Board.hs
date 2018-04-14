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
                     game_type :: String,
                     buttons :: [Button],
                     running :: Bool }


initWorld :: Int -> Int -> [(Position, Col)] -> Col -> Col -> String -> Bool -> IO World
initWorld size target history turn player game_type running = if running
                                                  then return $ World (initBoard size target history) turn player game_type (gameButtons size) running
                                                  else return $ World (initBoard size target history) turn player game_type (loadingButtons size) running

-- List of all buttons that the game uses.
gameButtons :: Int -> [Button]
gameButtons s = adjustButtons s [undoButton, saveButton, loadButton]

-- List of all buttons that the game uses.
loadingButtons :: Int -> [Button]
loadingButtons s = adjustButtons s [sizeSet5Button, sizeSet10Button, sizeSet19Button, targetSet3Button, targetSet5Button, targetSet7Button, whiteButton, blackButton, normalGameButton, fourAndFourGameButton]

getSize size = (fromIntegral ( ((size - 1) * spacing)) / 2)

adjustButtons :: Int -> [Button] -> [Button]
adjustButtons size = map adjustB
  where adjustB b = b { topLeft = (((fst (topLeft b) - round(getSize size)) - 40), ((snd (topLeft b) + round(getSize size)))),
                       bottomRight = (((fst (bottomRight b) - round(getSize size)) - 40), ((snd (bottomRight b) + round (getSize size))))
                      }

-- A function that returns to the last turn of the current player, thus, it actually undoes 2 turns.
undo :: Int -> IO World -> IO World
undo a world = do w <- world
                  if (checker w || (checkWon (board w) == Nothing))
                     then return w
                     else return $ (w { board = (board w) { pieces = remove (pieces (board w)) 2 } })
    where checker w = checkIfFirstTurn (pieces (board w))

--buttons for the loading screen

sizeSet5Button :: Button
sizeSet5Button = Button { topLeft = (-80, 130), bottomRight = (70, 100), value = "Set Size 5", action = (setSize 5) }
sizeSet10Button :: Button
sizeSet10Button = Button { topLeft = (80, 130), bottomRight = (230, 100), value = "Set Size 10", action = (setSize 10) }
sizeSet19Button :: Button
sizeSet19Button = Button { topLeft = (240, 130), bottomRight = (390, 100), value = "Set Size 19", action = (setSize 19) }
targetSet3Button :: Button
targetSet3Button = Button { topLeft = (-80, 90), bottomRight = (70, 60), value = "Set Target 3", action = (setTarget 3) }
targetSet5Button :: Button
targetSet5Button = Button { topLeft = (80, 90), bottomRight = (230, 60), value = "Set Target 5", action = (setTarget 5) }
targetSet7Button :: Button
targetSet7Button = Button { topLeft = (240, 90), bottomRight = (390, 60), value = "Set Target 7", action = (setTarget 7) }
whiteButton :: Button
whiteButton = Button { topLeft = (-80, 50), bottomRight = (70, 20), value = "Play as White", action = (setWhite 0) }
blackButton :: Button
blackButton = Button { topLeft = (80, 50), bottomRight = (230, 20), value = "Play as Black", action = (setBlack 0) }
normalGameButton :: Button
normalGameButton = Button { topLeft = (-80, 10), bottomRight = (70, -20), value = "Start Normal Game", action = (initNormal 0) }
fourAndFourGameButton :: Button
fourAndFourGameButton = Button { topLeft = (80, 10), bottomRight = (230, -20), value = "Start 4x4 Game", action = (initFourAndFour 0) }

setSize :: Int -> IO World -> IO World
setSize a w = do world <- w
                 (return $ world { board = (board world) { size = a } })

setTarget :: Int -> IO World -> IO World
setTarget a w = do world <- w
                   (return $ world { board = (board world) { target = a } })

setWhite :: Int -> IO World -> IO World
setWhite _ w = do world <- w
                  (return $ world { player = White })

setBlack :: Int -> IO World -> IO World
setBlack _ w = do world <- w
                  (return $ world { player = Black })

initNormal :: Int -> IO World -> IO World
initNormal _ w = do world <- w
                    (initWorld (size (board world)) (target (board world)) [] Black (player world) ("normal") True)

initFourAndFour :: Int -> IO World -> IO World
initFourAndFour _ w = do world <- w
                         (initWorld (size (board world)) (target (board world)) [] Black (player world) ("4x4") True)
-- A Button that rolls back one turn for the current player
undoButton :: Button
undoButton = Button { topLeft = (-80, 0), bottomRight = (0, -30), value = "Undo Move", action = (undo 0) }


-- A button to save the game
saveButton :: Button
saveButton = Button { topLeft = (-80, -40), bottomRight = (0, -70), value = "Save Game", action = save }

save :: IO World -> IO World
save w = do world <- w
            writeFile "save.dat" (show (size (board world)) ++ " " ++ show (target (board world)) ++ " " ++ (game_type world)) --Overwrite the old save file and write the size and target of the current board
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
            initWorld new_size new_target new_ps (turn world) (player world) new_game_type True
         where f = readFile "save.dat" -- Read in the raw save file data
               ls = splitOn "\n" (unsafePerformIO f) -- Split the save file into lines
               top = splitOn " " (head ls) -- Grab each word from the top line of the save file
               new_size = read (head top) :: Int -- First word of the top line is the saved game's board size
               new_target = read (top!!1) :: Int -- Second word of the top line is the saved game's target
               new_game_type = (top!!2)
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


-- An evaluation function for board. Given a board and a colour
-- return an integer (used float instead) indicating how good the board is for that colour.
evaluate :: Board -> Col -> Float
evaluate board col = (realToFrac(getNumConsecutive board col) + 17.5 * (getAverageLength board col) + (realToFrac(isWinningMove board col)) - (4 * realToFrac(getNumClosed board col)))
                    - (realToFrac(getNumConsecutive board (other col)) + 17.5 * (getAverageLength board (other col)) + realToFrac(isWinningMove board (other col)) - (4 * realToFrac(getNumClosed board (other col))))

--Checks for a potential win move and returns an overwhelming score
isWinningMove :: Board -> Col -> Int
isWinningMove board col = if (maxLength board col) >= ((target board)) && (potentialWin board col) then 20000
                          else 0

--Checks whether future moves can increase the size of the player's set to beyond the target, meaning the current move must be a win
potentialWin :: Board -> Col -> Bool
potentialWin board col = let newBoards = potentialBoards board col in
                          if (maximum(map (\b -> maxLength b col) newBoards)) > (target board) then True
                          else False

--Generates all potential board layouts for future moves
potentialBoards :: Board -> Col -> [Board]
potentialBoards board col = catMaybes (map (makeMove board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)])

-- Returns maximum length of set on board
maxLength :: Board -> Col -> Int
maxLength board col = maximum (map maximum (map (getLengths (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)]))

--Gets number of consecutive sets on the board
getNumConsecutive :: Board -> Col -> Int
getNumConsecutive board col = sum(map (getConsecutive (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)])

--Gets the number of closed off sets on the board
getNumClosed :: Board -> Col -> Int
getNumClosed board col = length (filter (== True) (getClosed board col (pieces board)))

--Gets the average length of a set on the board
getAverageLength :: Board -> Col -> Float
getAverageLength board col = if (getNumConsecutive board col) > 0
                             then realToFrac (sum(map sum (map (getLengths (pieces board) board col) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)])))
                                    / realToFrac (getNumConsecutive board col)
                             else 0.0

--Gets the consecutive sets on the board for a player
getConsecutive :: [(Position, Col)] -> Board -> Col -> (Int, Int) -> Int
getConsecutive [] _ _ _ = 0
getConsecutive (x:xs) board col dir | ((countBoardPieces x (pieces board) dir (size board) col) > 1) = 1 + (getConsecutive xs board col dir)
                                | otherwise = (getConsecutive xs board col dir)

--Gets the lengths of sets on the board that are greater than 1
getLengths :: [(Position, Col)] -> Board -> Col -> (Int, Int) -> [Int]
getLengths [] _ _ _ = [0]
getLengths (x:xs) board col dir | ((countBoardPieces x (pieces board) dir (size board) col) > 1) = (countBoardPieces x (pieces board) dir (size board) col) : (getLengths xs board col dir)
                                   | otherwise = (getLengths xs board col dir)


-- Gets the sets on the board that are 'closed' (i.e. sets with a length greater than 1 that have the other player's piece at the end)
getClosed :: Board -> Col -> [(Position, Col)] -> [Bool]
getClosed board col [] = [False]
getClosed board col (x:xs) = (map (isClosed board col x) [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0,0)]) ++ (getClosed board col xs)

--Checks whether a set is 'closed'
isClosed :: Board -> Col -> (Position, Col) -> (Int, Int) -> Bool
isClosed board col piece dir = if ((getSubsequentPiece board col piece dir) == Nothing) then False
                                  else True

--Gets the piece after the end of the player's set
getSubsequentPiece :: Board -> Col -> (Position, Col) -> (Int, Int) -> Maybe (Position, Col)
getSubsequentPiece board col piece dir = if (countBoardPieces piece (pieces board) dir (size board) col) > 1
                                            then (getPiece (pieces board) (addT (mulT ((countBoardPieces piece (pieces board) dir (size board) col)) dir) (fst (piece))))
                                         else Nothing

--Multiplies a number by a tuple
mulT :: Int -> (Int, Int) -> (Int, Int)
mulT x y = (x * (fst y), x * (snd y))

--Adds two tuples together
addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT x y = ((fst x) + (fst y), (snd x) + (snd y))

--for easy rule extension

getNumOpenEndedSets :: Board -> Col -> Int
getNumOpenEndedSets board col = undefined

-- checkFourAndFour :: Board -> C- (realToFrac(getNumConsecutive board (other col)) + 10 *(getAverageLength board (other col)) + 5 * realToFrac(maxLength board (other col)) + realToFrac(isWinningMove board (other col)) - (0.25 * realToFrac(getNumClol -> Position -> Bool
-- checkFourAndFour board col pos | countOpen board col pos == 2 = True
--                                  | otherwise = False

--countOpen :: Board -> Col -> Position -> Int
--countOpen board col pos =

checkFourAndFour :: Board -> Col -> Position -> Bool
checkFourAndFour board col pos | trace(show(countOpenAndClosed board col pos 4)) (countOpenAndClosed board col pos 4) >= 5 = True
                               | otherwise = False

--checkThreeAndThree board col pos = False

--countOpen :: Board -> Col -> Position -> Int
--countOpen board col pos =

countOpenAndClosed :: Board -> Col -> Position -> Int -> Int
countOpenAndClosed board col pos target = checkAllBoardPieces ((pos, col) : (pieces board)) ((pos, col) : (pieces board)) board target col


checkAllBoardPieces :: [(Position, Col)] -> [(Position, Col)] -> Board -> Int -> Col -> Int
checkAllBoardPieces [] _ _ _ _ = 0
-- Count pieces on the all 8 directions.
checkAllBoardPieces (x:xs) pb board tb color | (countBoardPieces x pb (0, 1) sb color) + (countBoardPieces x pb (0, -1) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- North & South
                                          | (countBoardPieces x pb (1, 0) sb color) + (countBoardPieces x pb (-1, 0) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- East & West
                                          | (countBoardPieces x pb (1, 1) sb color) + (countBoardPieces x pb (-1, -1) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- North East & South West
                                          | (countBoardPieces x pb (-1, 1) sb color) + (countBoardPieces x pb (1, -1) sb color) - 1 == tb = 1 + checkAllBoardPieces xs pb board tb color -- North West & South East
                                          | otherwise = 0 + checkAllBoardPieces xs pb board tb color
                                          where sb = size board
