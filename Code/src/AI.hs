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

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
--getBestMove ::  Int -- ^ Maximum search depth
--               -> GameTree -- ^ Initial game tree
--               -> Position
--getBestMove d tree = fst((next_moves tree)!! (getRandomIndex (length(next_moves tree))))
--getBestMove maxD tree =trace(show (fst((next_moves tree)!! (getRandomIndex (length(next_moves tree)))))) fst((next_moves tree)!! (getRandomIndex (length(next_moves tree))))
--displayPoss []=[]
--displayPoss (x:xs)=[fst x]++displayPoss xs

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove ::  Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
--getBestMove d tree = fst((next_moves tree)!! (getRandomIndex (length(next_moves tree))))
getBestMove maxD tree = trace(show (fst((next_moves tree)!! (getRandomIndex (length(next_moves tree)))))) fst((next_moves tree)!! (getRandomIndex (length(next_moves tree))))
displayPoss []=[]
displayPoss (x:xs)=[fst x]++displayPoss xs

getRandomIndex :: Int -- List length
                -> Int -- Pseudo-random index
getRandomIndex len  = fromIntegral((unsafePerformIO getCPUTime) `mod` toInteger(len))




--Gets list of empty positions (with no piece) on board for potential moves
getEmptyPos :: [(Position, Col)] --Pieces on board
            -> Int --Size of board
            -> [Position] --Empty positions on board
getEmptyPos [] size = range ((0, 0), (size - 1, size - 1))
getEmptyPos pieces size = range ((0, 0), (size - 1, size - 1)) \\ (map fst pieces)


--Generates list of possible positions to be used to construct GameTree
--Finds all legal moves for now TODO reduce size of moves to avoid combinatorial explosion
gen :: Board -> Col -> [Position]
gen board col = getEmptyPos (pieces board) (size board)


-- Function to update board by playing AI move (current search depth of 1 by default)
-- Makes a move using the board, colour and position.
-- Position is determined by building a list of empty moves (gen),
-- generating a tree from this list (buildTree), finding the best move in this GameTree
-- using getBestMove (currently just a random move) and making the move at this positions
-- using makeMove
updateBoard :: Board -> Col -> Maybe Board
updateBoard board colour = makeMove board colour (getBestMove 1 (buildTree gen board colour))

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> IO World -- ^ current world state
            -> IO (IO World)
updateWorld t world = do w <- world
                         if ((turn w) /= (player w) && checkWon (board w) == Nothing) --if not the player's turn
                            then return $ trace("turn " ++ show(turn w) ++ " ended") return w { board = fromJust(updateBoard (board w) (turn w)), turn = other (turn w) }
                         else return world



{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.
 At first, it is reasonable for this to be a random move!
 If both players are human players, the simple version above will suffice,
 since it does nothing.
 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}
