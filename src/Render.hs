module Render where

import Board
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Data.Array

squareSize :: Float
squareSize = 100

darkSquares :: Color
darkSquares  = makeColorI 126 84 41 255

lightSquares :: Color
lightSquares = makeColorI 240 234 225 255

promoteBkg :: Color
promoteBkg   = makeColorI 135 159 229 255

renderBoard :: BitmapData -> Board -> Picture
renderBoard bd board = Pictures [ renderSquare bd sq | sq <- assocs board ]

renderSquare :: BitmapData -> (Space,Square) -> Picture
renderSquare bd (sp,sq) = Pictures [renderSquareBkg sp,renderPiece bd (sp,sq)]

renderOnSpace :: Space -> Picture -> Picture
renderOnSpace (x,y) p = let
  px = squareSize * (fromIntegral x-4.5)
  py = squareSize * (fromIntegral y-4.5)
    in Translate px py p

renderSquareBkg :: Space -> Picture
renderSquareBkg s@(x,y) = let
  base = Polygon $ rectanglePath squareSize squareSize
  isWhite = (x+y) `mod` 2 == 1
  colored = Color (if isWhite then lightSquares else darkSquares) base
    in renderOnSpace s colored

renderPiece :: BitmapData -> (Space,Square) -> Picture
renderPiece _ (_,Nothing)  = Blank
renderPiece bd (sp,Just p) = renderOnSpace sp $ getPiece bd p

loadAsset :: IO BitmapData
loadAsset = do
    Just (Bitmap test) <- loadJuicyPNG "chess_pieces.png"
    return test


assetSquareSize :: Int
assetSquareSize = 133

getPiece :: BitmapData -> Piece -> Picture
getPiece bd (side,pt) = let
  x = case pt of
        King   -> 0
        Queen  -> 1
        Bishop -> 2
        Knight -> 3
        Rook   -> 4
        Pawn   -> 5
  y = case side of
        White -> 0
        Black -> 1
  in getSquare bd x y

getSquare :: BitmapData -> Int -> Int -> Picture
getSquare bd x y = let
  sq    = assetSquareSize
  rect = Rectangle (x*sq,y*sq) (sq,sq)
    in Scale 0.5 0.5 $ BitmapSection rect bd

markSpace :: Space -> Picture
markSpace sp = renderOnSpace sp mark

mark :: Picture
mark = Color red (circleSolid 20)

promotionButtons :: BitmapData -> Side -> Picture
promotionButtons bd side = let
  icons = Pictures [ renderPiece bd ((9,rank),Just (side,pt)) | (rank,pt) <-
      [(8,Queen),(7,Rook),(6,Bishop),(5,Knight)] ]
  bkg = Translate (squareSize*4.5) (squareSize*2) $
        Color promoteBkg $
        Polygon $ rectanglePath squareSize (4*squareSize)
  in Pictures [bkg,icons]













