module Draw(drawWorld) where

import Graphics.Gloss
import Board

spacing = 25

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = Translate f f (Pictures (drawBoard (board w) (0, 0) []))
            where f = (fromIntegral (- ((size (board w) - 1) * spacing)) / 2)

-- Draw the board for the Gomoku game.
drawBoard :: Board -> Position -> [Picture] -> [Picture]
drawBoard board (x, y) pics | x == ((size board) - 1) && y == ((size board) - 1) = drawPiece board (x, y) : pics
                            | x == ((size board) - 1) = line ((x, y), (x, y + 1)) : drawPiece board (x, y) : drawBoard board (0, y + 1) pics
                            | y == ((size board) - 1) = line ((x, y), (x + 1, y)) : drawPiece board (x, y) : drawBoard board (x + 1, y) pics
                            | otherwise = Pictures [line ((x, y), (x + 1, y)), line ((x, y), (x, y + 1))] : drawPiece board (x, y) : drawBoard board (x + 1, y) pics
                            where line ((x1, y1), (x2, y2)) = Color blue $ Line [(fromIntegral (x1 * spacing), fromIntegral (y1 * spacing)), (fromIntegral (x2 * spacing), fromIntegral (y2 * spacing))]


-- Draw the piece if there is a piece on the particular position.
drawPiece :: Board -> Position -> Picture
drawPiece b p = case getThePiece (pieces b) p of
                     Just piece -> col (snd piece) $ drawCircle p --if there is a piece on this position, draw it.
                     Nothing -> Color blue $ Translate (fromIntegral (fst p * spacing)) (fromIntegral (snd p * spacing)) $ ThickCircle 2 2
              where col Black = Color black
                    col White = Color white

-- Draw a Black or White chip in a given position
drawCircle :: Position -> Picture
drawCircle (x, y) = Translate (fromIntegral (x * spacing)) (fromIntegral (y * spacing)) $ ThickCircle 5 5
