module Board where

import Data.Array
import Data.Char
import Data.Maybe
import Data.Tuple

import Control.Monad.Writer.Lazy
import Control.Monad.State

type Board = Array Space Square

type Space = (Int,Int)
data Move = Move { msrc :: Space, mdest :: Space, mtaken :: Square }
          | Castle { castleSide :: KQ }
          | EnPasan { msrc :: Space, mdest :: Space}
          | Promotion { msrc :: Space, mdest :: Space, mtaken :: Square, becomes :: PieceType }
          deriving(Eq,Ord,Show)

data KQ = K | Q deriving(Eq,Ord,Show,Enum)

type Castling = Array (Side,KQ) Bool

data Position = Pos {
  posBoard :: Board
 ,posToMove :: Side
 ,posCastlingRights :: Castling
 ,posLastMove :: Maybe Move
 ,posHmc :: Int
 ,posFmc :: Int
 ,posWhiteKing :: Space
 ,posBlackKing :: Space
                   }deriving(Eq,Ord,Show)

type Square = Maybe Piece

data Side  = White | Black deriving(Eq,Ord,Show,Enum)
type Piece = (Side,PieceType)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving(Eq,Ord,Show)

instance Ix Side where
  range (ls,rs) = [ls..rs]
  index (ls,_) s = fromEnum s - fromEnum ls
  inRange (ls,rs) s = ls <= s && s <= rs

instance Ix KQ where
  range (lkq,rkq) = [lkq..rkq]
  index (lkq,_) kq = fromEnum kq - fromEnum lkq
  inRange (lkq,rkq) kq = lkq <= kq && kq <= rkq

emptyBoard :: Board
emptyBoard = listArray ((1,1),(8,8)) (replicate 64 Nothing)

starting :: Position
starting = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

parseFEN :: String -> Position
parseFEN fen = let
  [board',toMove',castling',lastMove',hmc',fmc'] = words fen
  board = parseBoardFEN board'
  toMove = case toMove' of
             "w" -> White
             "b" -> Black
             _   -> error $ "badToMove: " ++ toMove'
  castling = parseCastling castling'
  lastMove = parseMove board lastMove'
  wk= fromJust $ lookup (Just (White,King)) (map swap$assocs board)
  bk= fromJust $ lookup (Just (Black,King)) (map swap$assocs board)
  hmc = read hmc'
  fmc = read fmc'
    in Pos{
      posBoard=board,
      posToMove=toMove,
      posCastlingRights=castling,
      posLastMove=lastMove,
      posHmc=hmc,
      posFmc=fmc,
      posWhiteKing=wk,
      posBlackKing=bk
        }

parseCastling :: String -> Castling
parseCastling w = listArray ((White,K),(Black,Q)) [ char `elem` w | char <- "KQkq" ]

parseBoardFEN :: String -> Board
parseBoardFEN w = let
    pieces :: [(Space,Square)]
    pieces =  evalState (execWriterT $ mapM_ parseBoardFENChar w) (1,8)
      in emptyBoard // pieces

parseBoardFENChar :: Char -> WriterT [(Space,Square)] (State Space) ()
parseBoardFENChar c
  | isDigit c = do
      (f,r) <- get
      put (f+read[c],r)
  | c == '/'  = do
      (_,r) <- get
      put (1,r-1)
  | otherwise = do
      s@(f,r) <- get
      tell [(s,return$parsePiece c)]
      put (f+1,r)


parseMove :: Board -> String -> Maybe Move
parseMove _ "-" = Nothing
parseMove _  _ = undefined

parseSpace :: String -> Space
parseSpace [f,r] = (fromIntegral $ ord f -96,fromIntegral $ ord r-48)
parseSpace space =  error $ "bad space: " ++ space

parsePiece :: Char -> Piece
parsePiece c = (if isLower c then Black else White,case toLower c of
                                                  'p' -> Pawn
                                                  'r' -> Rook
                                                  'n' -> Knight
                                                  'b' -> Bishop
                                                  'q' -> Queen
                                                  'k' -> King
                                                  _   -> error $ "bad piece type: " ++ [c]
               )




sketchPos :: Position -> String
sketchPos =  quickSketch . posBoard

quickSketch :: Board -> String
quickSketch board = unlines [[ sketchSquare (board!(f,r)) | f<-[1..8] ] | r <- [8,7..1]]

sketchSquare :: Square -> Char
sketchSquare Nothing          = ' '
sketchSquare (Just (side,pt)) = (if side == White then toUpper else id) c
  where
    c =  case pt of
            Pawn   -> 'p'
            Rook   -> 'r'
            Knight -> 'n'
            Bishop -> 'b'
            Queen  -> 'q'
            King   -> 'k'

to :: Position -> Space -> Space -> Move
to pos src dest = Move{msrc=src,mdest=dest,mtaken=posBoard pos!dest}

showSpace :: Space -> String
showSpace (x,y) = showFile x:show y

showFile :: Int -> Char
showFile x = chr (96+x)

