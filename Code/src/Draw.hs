module Draw(drawWorld) where

import Graphics.Gloss
import Board

half :: World -> Float
half w = (fromIntegral (- ((size (board w) - 1) * spacing)) / 2) -- Find half-size of board to centre it

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: IO World -> IO Picture
drawWorld world = do w <- world
                     return $ Pictures ( Translate (half w) (half w) (Pictures (drawBoard (board w) (0, 0) [])) : (Pictures (drawButtons w (buttons w))) : checkEnd w)

-- Draw the board for the Gomoku game.
drawBoard :: Board -> Position -> [Picture] -> [Picture]
drawBoard board (x, y) pics | x == ((size board) - 1) && y == ((size board) - 1) = drawPiece board (x, y) : pics
                            | x == ((size board) - 1) = line ((x, y), (x, y + 1)) : drawPiece board (x, y) : drawBoard board (0, y + 1) pics
                            | y == ((size board) - 1) = line ((x, y), (x + 1, y)) : drawPiece board (x, y) : drawBoard board (x + 1, y) pics
                            | otherwise = Pictures [line ((x, y), (x + 1, y)), line ((x, y), (x, y + 1))] : drawPiece board (x, y) : drawBoard board (x + 1, y) pics
                            where line ((x1, y1), (x2, y2)) = Color blue $ Line [(fromIntegral (x1 * spacing), fromIntegral (y1 * spacing)), (fromIntegral (x2 * spacing), fromIntegral (y2 * spacing))]
                            --line ((x1, y1), (x2, y2)) draws a blue line from (x1, y1) to (x2, y2).


-- Draw the piece if there is a piece on the particular position.
-- If the condition is satisfied, draw the piece by calling the drawCircle function with the particular colour.
drawPiece :: Board -> Position -> Picture
drawPiece b p = case getPiece (pieces b) p of
                     Just piece -> col (snd piece) $ drawCircle p --if there is a piece on this position, draw it.
                     Nothing -> Color blue $ Translate (fromIntegral (fst p * spacing)) (fromIntegral (snd p * spacing)) $ ThickCircle 2 2
              where col Black = Color black
                    col White = Color white


-- Draw a Black or White chip in a given position
drawCircle :: Position -> Picture
drawCircle (x, y) = Translate (fromIntegral (x * spacing)) (fromIntegral (y * spacing)) $ ThickCircle 7 7

-- Check whether the game is finished or not. If so, print out the winner of the game on the board.
checkEnd :: World -> [Picture]
checkEnd w = case checkWon (board w) of
                  Just c  -> [printOutWinner w c]
                  Nothing -> []

scaler = 0.4

-- Print out the winner.
printOutWinner :: World -> Col -> Picture
printOutWinner w White = color white $ Translate (half w) 0 (scale scaler scaler (text "White Wins"))
printOutWinner w Black = color black $ Translate (half w) 0 (scale scaler scaler (text "Black Wins"))

-- This function draws the button on the board.
drawButtons :: World -> [Button] -> [Picture]
drawButtons w = map drawB
  -- Move the button to the correct position
  where drawB b = Translate (fromIntegral $
                    (fst (topLeft b) - fst (bottomRight b)) `div` 2 + fst (bottomRight b))
                  (fromIntegral $
                    (snd (topLeft b) - snd (bottomRight b)) `div` 2 + snd (bottomRight b))
                  (Pictures (
                   -- Draw a white rectangle for the button. Then write the text with a black colour.
                   color white (polygon (rectanglePath 80 30)) : [color black $ Translate (-32) (-5) (scale 0.1 0.1 (text $ value b))]
                  ))
