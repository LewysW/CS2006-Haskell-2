module Draw(drawWorld, drawLoading) where

import Graphics.Gloss
import Board

import Debug.Trace

half :: World -> Float
half w = (fromIntegral (- (( size (board w) - 1) * spacing)) / 2) -- Find half-size of board to centre it

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.

drawLoading :: IO World -> IO Picture
drawLoading world = do w <- world
                       return $ Pictures ( (Pictures (drawButtonsForLoading w (150, 30) (buttons w))) : [welcomeToGomoku w] )

drawWorld :: IO World -> IO Picture
drawWorld world = do w <- world
                     game_images <- sequence $ loadBMP "./Assets/hint_piece.bmp" : loadBMP "./Assets/black_piece.bmp" : loadBMP "./Assets/white_piece.bmp" : []
                     button_images <- sequence $ loadBMP "./Assets/undo_button.bmp" : loadBMP "./Assets/save_button.bmp" : loadBMP "./Assets/load_button.bmp" : loadBMP "./Assets/hint_button.bmp" : []
                     winscreen_images <- sequence $ loadBMP "./Assets/black_win.bmp" : loadBMP "./Assets/white_win.bmp" : []
                     if (running w)
                        then return $ Pictures ( Translate (half w) (half w) (Pictures (drawBoard (board w) (0, 0) [] game_images)) : (Pictures (drawButtons (buttons w) [] button_images)) : (checkEnd w winscreen_images))
                     else (drawLoading world)


-- Draw the board for the Gomoku game.
drawBoard :: Board -> Position -> [Picture] -> [Picture] -> [Picture]
drawBoard board (x, y) pics [] = pics
drawBoard board (x, y) pics bmps | x == ((size board) - 1) && y == ((size board) - 1) = drawPiece board (x, y) bmps : pics
                                 | x == ((size board) - 1) = line ((x, y), (x, y + 1)) : drawPiece board (x, y) bmps : drawBoard board (0, y + 1) pics bmps
                                 | y == ((size board) - 1) = line ((x, y), (x + 1, y)) : drawPiece board (x, y) bmps : drawBoard board (x + 1, y) pics bmps
                                 | otherwise = Pictures [line ((x, y), (x + 1, y)), line ((x, y), (x, y + 1))] : drawPiece board (x, y) bmps : drawBoard board (x + 1, y) pics bmps
                                 where line ((x1, y1), (x2, y2)) = Color black $ Line [(fromIntegral (x1 * spacing), fromIntegral (y1 * spacing)), (fromIntegral (x2 * spacing), fromIntegral (y2 * spacing))]



-- Draw the piece if there is a piece on the particular position.
-- If the condition is satisfied, draw the piece by calling the drawCircle function with the particular colour.
drawPiece :: Board -> Position -> [Picture] -> Picture
drawPiece b p bmps = case getPiece (pieces b) p of
                          Just piece -> drawCircle p (snd piece) "nothint" bmps
                          Nothing -> if (p == (hint b) && ((hints b)))
                                        then (drawCircle p Black "hint" bmps)
                                     else (drawCircle p Black "blue" bmps)
          where col Black = Color black
                col White = Color white


-- Draw a Black or White chip in a given position
drawCircle :: Position -> Col -> String -> [Picture] -> Picture
drawCircle (x, y) Black "hint" bmps = drawImage (x, y) (bmps!!0)
drawCircle (x, y) Black "nothint" bmps = drawImage (x, y) (bmps!!1)
drawCircle (x, y) White "nothint" bmps = drawImage (x, y) (bmps!!2)
drawCircle (x, y) Black "blue" bmps = (Color blue $ Translate (fromIntegral (x * spacing)) (fromIntegral (y * spacing)) $ ThickCircle 2 2)

-- Check whether the game is finished or not. If so, print out the winner of the game on the board.
checkEnd :: World -> [Picture] -> [Picture]
checkEnd w bmps = case checkWon (board w) of
                       Just c  -> [printOutWinner c bmps]
                       Nothing -> []

-- Draw the image on the given position.
drawImage :: Position -> Picture -> Picture
drawImage (x, y) bmp = Translate (fromIntegral (x * spacing)) (fromIntegral (y * spacing)) $ bmp

scaler = 0.4

-- Print out the winner.
printOutWinner :: Col -> [Picture] -> Picture
printOutWinner Black bmps = drawImage (0, 0) (bmps!!0)
printOutWinner White bmps = drawImage (0, 0) (bmps!!1)

welcomeToGomoku :: World -> Picture
welcomeToGomoku w = color white $ Translate (-300) (-150) (scale scaler scaler (text "Welcome To Gomoku!"))

drawButtonsForLoading :: World -> (Float, Float) -> [Button] -> [Picture]
drawButtonsForLoading w size = map drawB
  -- Move the button to the correct position
  where drawB b = Translate (fromIntegral $
                    (fst (topLeft b) - fst (bottomRight b)) `div` 2 + fst (bottomRight b))
                  (fromIntegral $
                    (snd (topLeft b) - snd (bottomRight b)) `div` 2 + snd (bottomRight b))
                  (Pictures (
                   -- Draw a white rectangle for the button. Then write the text with a black colour.
                   color white (polygon (rectanglePath (fst(size)) (snd(size)))) : [color black $ Translate (-1 * (fst(size)) / 2.5) (-5) (scale 0.1 0.1 (text $ value b))]
                  ))

-- This function draws the button on the board.
drawButtons :: [Button] -> [Picture] -> [Picture] -> [Picture]
drawButtons [] ps [] = ps
drawButtons btns ps bmps = (Translate btn_x btn_y $ (head bmps)) : drawButtons (tail btns) ps (tail bmps)
            where b = head btns
                  btn_x = (fromIntegral $ (fst (topLeft b) - fst (bottomRight b)) `div` 2 + fst (bottomRight b))
                  btn_y = (fromIntegral $ (snd (topLeft b) - snd (bottomRight b)) `div` 2 + snd (bottomRight b))
