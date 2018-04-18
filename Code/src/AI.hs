module AI where

import Board

--Imported for use in AI
import Data.Ix
import Data.List ((\\))
import Data.Maybe
import Data.Fixed
import Debug.Trace --should erase
import Data.List (sortBy)
import Data.Function (on)

--Used as pseudo random number generator seed
import System.CPUTime
import System.IO.Unsafe

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player


-- Gets the best move for the AI by returning the position with the maximum score
-- determined by the evaluate function. If the middle is empty at the beginning of the
-- game then the AI will always place there.
-- @Int - search depth for tree, @tree - game tree containing board moves.
-- @Position - position for AI to use.
getBestMove ::  Int
               -> GameTree
               -> Position
getBestMove d tree = let middle = getMiddleOfBoard (size (game_board tree)) in
                     if (getPiece (pieces (game_board tree)) middle) == Nothing then middle
                     else if (length(pieces (game_board tree)) == 1) && ((getPiece (pieces (game_board tree)) middle) /= Nothing)
                         then addT middle (-1, -1)
                     else case (maxTurn (next_moves tree) (game_turn tree)) of
                           Just m -> fst m
                           Nothing -> middle

--Works out the middle coordinate of the board.
-- @Int - size of board, @Position - position of middle piece.
getMiddleOfBoard :: Int -> Position
getMiddleOfBoard size = (size `div` 2, size `div` 2)

--Returns the position and associated game tree with the max score determined by the
-- evaluate function. Returns nothing if given an empty list.
-- @[(Position, GameTree)] - list of current board positions and trees, @Col - colour to
-- find best move for, @Maybe (Position, GameTree) - best position to place a piece and
-- associated game tree if there is a best postion.
maxTurn :: [(Position, GameTree)] -> Col -> Maybe (Position, GameTree)
maxTurn [] _ = Nothing
maxTurn nextMoves col = Just (snd(head(sortByScore (evaluateNextMoves nextMoves col))))

--Sorts the list of evaluated positions by score and returns the list.
-- @[(Float, (Position, GameTree))] - list of scores and associated positions/game trees.
-- @@[(Float, (Position, GameTree))] - sorted list of positions/game trees by score
sortByScore :: [(Float, (Position, GameTree))] -> [(Float, (Position, GameTree))]
sortByScore = sortBy (flip compare `on` fst)

--Evaluates all moves to return a list of moves and associated scores based on
-- how good the board is for a given player.
-- @[(Position, GameTree)] - list of positions to move to and associated game trees,
-- @Col - colour to use in board evaluation.
-- @[(Float, (Position, GameTree))] - float score and related position/gametree
evaluateNextMoves :: [(Position, GameTree)] -> Col -> [(Float, (Position, GameTree))]
evaluateNextMoves nextMoves col = map (\ nextMove -> (evaluate (game_board (snd nextMove)) col, nextMove)) nextMoves

--Used for random AI.
--Uses CPU time to get a pseudo random number and this number is divided by the length
-- of the list of moves on the board to get a remainder representing a
-- random board position index.
-- @Int - length of list of available board moves.
-- @Int - Pseudo-random index used to get a random move.
getRandomIndex :: Int -- List length
                -> Int -- Pseudo-random index
getRandomIndex 0 = 0
getRandomIndex len = fromIntegral((unsafePerformIO getCPUTime) `mod` toInteger(len))

--Gets list of empty positions (with no piece) on board for potential moves. Does
-- this by filtering out the coordinates (using \\ operator) that match coordinates
-- in the list of pieces on the board.
-- @[(Position, Col)] - pieces on the board, @Int - size of board,
-- @[Position] - empty positions on board.
getEmptyPos :: [(Position, Col)] --Pieces on board
            -> Int --Size of board
            -> [Position] --Empty positions on board
getEmptyPos [] size = range ((0, 0), (size - 1, size - 1))
getEmptyPos pieces size = range ((0, 0), (size - 1, size - 1)) \\ (map fst pieces)

--Generates list of available game positions by getting all of the empty positoins on the board.
-- @Board - board to check for available positions, @Col - colour of player.
-- @[Position] - available positions.
gen :: Board -> Col -> [Position]
gen board col = trace("gen") getEmptyPos (pieces board) (size board)


-- Function to update board by playing AI move (current search depth of 1 by default)
-- Makes a move using the board, colour and position.
-- Position is determined by building a list of empty moves (gen),
-- finding the score of each of the moves in this list,
-- using getBestMove and making the move at this position
-- using makeMoves
-- @World - world containing board to update. @Col - colour to use to make move
-- @String - string representing the AI mode. @Position - best position
-- @Maybe World - updated world.
updateBoard :: World -> Col -> String -> Position -> Maybe World
updateBoard w colour ai bestPos = let positions = (gen (board w) colour) in
                                  case ai of
                                  "beginner" -> case (getRandomPosition (getRandomIndex (length(positions))) (positions)) of
                                    Just c -> case (makeMove (board w) colour c) of
                                      Just b -> Just (w {board = b, isUpdated = True, turn = other (turn w)})
                                      Nothing -> Just (w {isUpdated = True})
                                    Nothing -> Just (w {isUpdated = True})
                                  "intermediate" -> case (makeMove (board w) colour bestPos) of
                                    Just b -> if (not (isUpdated w)) || (pieces (board w)) == []
                                                       then Just (w {board = b, isUpdated = True, turn = other (turn w)})
                                              else Just w
                                    Nothing -> Nothing

--Gets a random position using a given random index
-- @Int - random index, @[Position] - list of empty positions,
-- @Maybe Position - position randomly selected (if not Nothing)
getRandomPosition :: Int -> [Position] -> Maybe Position
getRandomPosition _ [] = Nothing
getRandomPosition index moves = Just((moves)!!(index))


-- Update the world state after some time has passed
--Returns the current world if it's the player's turn, otherwise updates the world
-- using the functions to select and play an AI move.
-- @Float - time since last update, @IO World - current world state,
-- @IO (IO World) - updated world.
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> IO World -- ^ current world state
            -> IO (IO World)
updateWorld t world = do wo <- world
                         let bestMove = (getBestMove 1 (buildTree gen (board wo) (player wo)))

                         if (((turn wo) /= (player wo)) && ((checkWon (board wo)) == Nothing) && (((ai wo)) /= ("pvp"))) --if not the player's turn
                           then case (updateBoard wo (turn wo) (ai wo) bestMove) of
                             Just w -> return $ trace("turn " ++ show(turn wo) ++ " ended") return w
                             Nothing ->return $ trace("turn " ++ show(turn wo) ++ " ended") return (trace("testing") wo)
                         else
                           return $ return wo { board = (board wo) { hint = bestMove } }
