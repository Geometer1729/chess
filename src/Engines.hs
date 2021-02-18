module Engines where

import Board
import Moves
import System.Random

type Engine = Position -> IO Move
-- IO is used to allow randomness file and reading

randomMover :: Engine
randomMover pos = do
  let moves = legalMoves pos
  i <- randomRIO (0,length moves -1)
  return $ moves!!i

