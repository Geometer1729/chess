module Engines where

import Board
import Moves

import Control.Monad.State
import Data.Array
import Data.List
import Data.Maybe
import System.Random
import Data.Ord
import Control.Concurrent

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
  if null moves
     then error "No legal moves but game not ended"
     else do
        i <- randomRIO (0,length moves -1)
        return $ moves!!i

data GameTree = Node{
  gtPos   :: Position,
  gtEval  :: Maybe Eval,
  gtAlpha :: Eval,
  gtBetta :: Eval,
  gtMoves :: [(Move,GameTree)]
    } | Leaf Result

engine :: Engine
engine pos = do
  let tree = createTree pos
  let start = (0,tree)
  (depth,refinedTree) <- anyTime 10 deepen start
  putStr "ran at depth of:"
  print depth
  return $ getMove refinedTree

getMove :: GameTree -> Move
getMove = fst . head . gtMoves

anyTime :: Int -> (a -> IO a) -> a -> IO a
anyTime time f x = do
  mx <- newMVar x
  threadID  <- forkIO (forever $ refine mx f)
  threadDelay (time*1000000)
  result <- readMVar mx
  killThread threadID
  return result

refine :: MVar a -> (a -> IO a) -> IO ()
refine mvar f = do
    x <- readMVar mvar
    x' <- f x
    _ <- swapMVar mvar x'
    return ()

deepen :: (Int,GameTree) -> IO (Int,GameTree)
deepen (depth,tree) = do
  let newDepth = depth+1
  let newTree  = evaluate newDepth tree
  putStrLn "at depth of: "
  print newDepth
  putStrLn "favorite move:"
  print $ getMove newTree
  putStrLn "Eval is:"
  print $ getEval newTree
  return (newDepth,newTree)

createTree :: Position -> GameTree
createTree pos = let
  moves = legalMoves pos
  maybeRes = scoreGame pos moves
  noScore = Node {
            gtPos   = pos,
            gtEval  = Nothing,
            gtAlpha = End (Win Black),
            gtBetta = End (Win White),
            gtMoves = [ (move,createTree $ doMove move pos) | move <- moves ]
                        }
    in maybe noScore Leaf maybeRes

staticEval :: Position -> Eval
staticEval pos = let
  toMove = posToMove pos
  pieces = mapMaybe snd . assocs . posBoard $ pos
  materialScore = sum $ map scorePiece pieces
  checkPenalty = if isCheck toMove pos then (case toMove of
                                       White -> -0.5
                                       Black -> 0.5
                                            )
                                       else 0
    in Score $ materialScore + checkPenalty

scorePiece :: Piece -> Float
scorePiece (side,pt) = let
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

--- White maximizes
--- Black minimizes
--- alpha: white is assured at least
--- betta: black is assured at most

evaluate :: Int -> GameTree -> GameTree
evaluate _     (Leaf res) = Leaf res
evaluate 0     node@Node{gtPos=pos} = let
  static = staticEval pos
    in case posToMove pos of
         White -> node{
            gtEval=Just static,
            gtAlpha = max (gtAlpha node) static
                      }
         Black -> node{
            gtEval=Just static,
            gtBetta = min (gtBetta node) static
                      }
evaluate depth gt@Node{
  gtPos = pos,
  gtMoves=moves,
  gtAlpha=alpha,
  gtBetta=betta
                        } = let
                        toMove = posToMove pos
                        (moves',(alpha',betta')) = runState ( mapM ((case toMove of
                                                                       White -> evalScanerWhite
                                                                       Black -> evalScanerBlack
                                                                         ) depth) moves ) (alpha,betta)
                        sortedMoves = case toMove of
                                        White -> sortOn (       getEval.snd) moves'
                                        Black -> sortOn (Down . getEval.snd) moves'
                        newEval  = getEval (snd . head $ sortedMoves)
                        newAlpha = case toMove of
                                     White -> alpha'
                                     Black -> alpha
                        newBetta = case toMove of
                                     White -> betta
                                     Black -> betta'
                             in gt{
                              gtEval =Just newEval,
                              gtAlpha=newAlpha,
                              gtBetta=newBetta,
                              gtMoves=sortedMoves
                                  }



evalScanerWhite :: Int -> (Move,GameTree) -> State (Eval,Eval) (Move,GameTree)
evalScanerWhite _ (mv,l@(Leaf res)) = do
  (alpha,betta) <- get
  put (max alpha (End res),betta)
  return (mv,l)
evalScanerWhite depth (mv,gt@Node{}) = do
    (alpha,betta) <- get
    if alpha >= betta
       then return (mv,gt{gtAlpha = max alpha (gtAlpha gt),gtEval=Just alpha})
       else do
          let gt' = evaluate (depth-1) gt{gtAlpha=max alpha (gtAlpha gt)}
          put (gtAlpha gt',betta)
          return (mv,gt')

evalScanerBlack :: Int -> (Move,GameTree) -> State (Eval,Eval) (Move,GameTree)
evalScanerBlack _ (mv,l@(Leaf res)) = do
  (alpha,betta) <- get
  put (alpha,min betta (End res))
  return (mv,l)
evalScanerBlack depth (mv,gt@Node{}) = do
    (alpha,betta) <- get
    if alpha >= betta
       then return (mv,gt{gtBetta = min betta (gtBetta gt),gtEval=Just betta})
       else do
          let gt' = evaluate (depth-1) gt{gtBetta=min betta (gtBetta gt)}
          put (alpha,gtBetta gt')
          return (mv,gt')

getEval :: GameTree -> Eval
getEval (Leaf res) = End res
getEval Node{gtEval=Just eval} = eval
getEval Node{gtEval=Nothing} = error "eval not defined"
