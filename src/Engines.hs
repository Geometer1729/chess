module Engines where

import Board
import Moves
import System.Random
import Data.Maybe
import Data.Array

type Engine = Position -> IO Move
-- IO is used to allow randomness file and reading

data Eval = Score Float | End Result deriving(Eq,Show)
-- High is good for white

instance Ord Eval where
  compare (Score x) (Score y) = compare x y
  compare (End x) (End y) = compare x y
  compare (Score _) (End (Win White)) = LT
  compare (Score x) (End Draw) = compare x 0
  compare (Score _) (End (Win Black)) = GT
  compare l@(End _) r@(Score _) = flipOrdering $ compare r l

flipOrdering :: Ordering -> Ordering
flipOrdering = compare EQ

randomMover :: Engine
randomMover pos = do
  let moves = legalMoves pos
  i <- randomRIO (0,length moves -1)
  return $ moves!!i

data GameTree = Node Position [(Move,GameTree)] | Leaf Result

engine :: Engine
engine pos = let
  tree = createTree pos
  (maybeMove,_) = alphaBeta 3 (End $ Win Black) (End $ Win White) tree
    in case maybeMove of
         Just move -> return move
         Nothing -> error "alphaBeta called with either a mate or depth 0"

createTree :: Position -> GameTree
createTree pos = let
  moves = legalMoves pos
  maybeRes = scoreGame pos moves
    in case maybeRes of
         Just res -> Leaf res
         Nothing -> Node pos [ (move,createTree $ doMove move pos) | move <- moves ]

staticEval :: Position -> Eval
staticEval pos = let
  pieces = mapMaybe snd . assocs . posBoard $ pos
    in Score $ sum $ map scoreMaterial pieces

scoreMaterial :: Piece -> Float
scoreMaterial (side,pt) = let
  sideMult = case side of
               Black -> -1
               White -> 1
  value = case pt of
            Pawn -> 1
            Knight -> 3
            Bishop -> 3
            Rook -> 5
            Queen  -> 9
            King -> 0
                           in sideMult*value

  {-
    alpha is worst case for white
    beta is worst case for black
  -}

alphaBeta :: Int -> Eval -> Eval -> GameTree -> (Maybe Move,Eval)
alphaBeta _ _ _ (Leaf result) = (Nothing,End result)
alphaBeta 0 _ _ (Node pos _) = (Nothing,staticEval pos)
alphaBeta _ _ _ (Node _ [])  = error "should be unreachable"
alphaBeta depth alpha beta  (Node _ [(move,newTree)]) = let
  (_,eval)   = alphaBeta (depth-1) alpha beta newTree
       in (Just move,eval)
alphaBeta depth alpha beta (Node pos ((move,newTree):moves)) = let
  toMove = posToMove pos
  (_,eval)   = alphaBeta (depth-1) alpha beta newTree
    in case toMove of
        White -> let
          alpha' = max alpha eval
            in if beta <= alpha'
                  then (Just move,eval)
                  else let
                      (laterMove,laterEval) = alphaBeta depth alpha' beta (Node pos moves)
                        in if eval >= laterEval
                              then (Just move,eval)
                              else (laterMove,laterEval)

        Black -> let
          beta' = min beta eval
            in if beta' <= alpha
                  then (Just move,eval)
                  else let
                    (laterMove,laterEval) = alphaBeta depth alpha beta' (Node pos moves)
                      in if eval <= laterEval
                            then (Just move,eval)
                            else (laterMove,laterEval)











