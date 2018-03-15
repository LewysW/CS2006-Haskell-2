module Board where

data Col = Black | White
  deriving (Show Eq)

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

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 []

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).

data World = World { board :: Board,
                     turn :: Col }

initWorld = World initBoard Black


-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
-- If the movement makes the piece to go out of bounds of the board, return Nothing.
-- Otherwise, add the new piece on the board.
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = case getThePiece (pieces board) position of
  Just x -> Nothing -- Position already occupied
  Nothing -> if ((fst position < 0) || (fst position > size board - 1) || (snd position < 0) || (snd position > size board - 1) )
                then Nothing
                else Just board { pieces = (position, colour) : pieces board }


-- Get the current piece at the given position.
-- If there is no corresponding piece, return Nothing.
getThePiece :: [(Position, Col)] -> Position -> Maybe (Position, Col)
getThePiece [] position = Nothing
getThePiece (x:xs) position | fst x == position = Just x
                            | otherwise = getThePiece xs position


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board = checkPiecesOnTheBoard (pieces board) board


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
checkPiecesOnTheBoard :: [(Position, Col)] -> Board -> Maybe Col
checkPiecesOnTheBoard (x:xs) board | countPiecesOnTheBoard x pb (0, 1) sb (snd x) == tb = Just (snd x) -- N
                                   | countPiecesOnTheBoard x pb (0, -1) sb (snd x) == tb = Just (snd x) -- S
                                   | countPiecesOnTheBoard x pb (1, 0) sb (snd x) == tb = Just (snd x) -- E
                                   | countPiecesOnTheBoard x pb (-1, 0) sb (snd x) == tb = Just (snd x) -- W
                                   | countPiecesOnTheBoard x pb (1, 1) sb (snd x) == tb = Just (snd x) -- NE
                                   | countPiecesOnTheBoard x pb (-1, 1) sb (snd x) == tb = Just (snd x) -- NW
                                   | countPiecesOnTheBoard x pb (1, -1) sb (snd x) == tb = Just (snd x) -- SE
                                   | countPiecesOnTheBoard x pb (-1, -1) sb (snd x) == tb = Just (snd x) -- SW
                                   | otherwise = checkPiecesOnTheBoard xs board
                                   where pb = pieces board
                                         sb = size board
                                         tb = target board


-- This function takes a piece, direction, size and colour, and count the number of the pieces in that direction.
-- Moreover, it is not allowed to go out of the bounds of the board.
-- Thus, this function should check the position of the piece is in the bound of the board.
countPiecesOnTheBoard :: (Position, Col) -> [(Position, Col)] -> (Int, Int) -> Int -> Col -> Int
countPiecesOnTheBoard (_, Black) _ _ _ White = 0
countPiecesOnTheBoard (_, White) _ _ _ Black = 0
countPiecesOnTheBoard x xs d size colour | (fst (fst x) < 0 || fst (fst x) >= size || snd (fst x) < 0 || snd (fst x) >= size) = 0
                                         | otherwise = case getThePiece xs (fst (fst x) + fst d, snd (fst x) + snd d) of
                                                            Just piece -> 1 + (countPiecesOnTheBoard piece xs d size colour)
                                                            Nothing -> 1


-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate board colour = undefined
