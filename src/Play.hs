{-# LANGUAGE MultiWayIf #-}
module Play where

import Board
import Moves
import Render
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import Data.Maybe
import Data.Array
import Engines

data GameState=GS{
  gspos :: Position,
  lastClick :: Maybe Space,
  promotion :: Maybe Move, -- stores promotion untill the user specifies what the pawn is being promoted to
  gsmoves :: [Move],
  gsWhite :: Maybe Engine,
  gsBlack :: Maybe Engine
                 }

playChess :: IO ()
playChess = do
  asset <- loadAsset
  playIO
    (InWindow "chess" (1000,1000) (500,500))
    black
    1
    startingGS
    (return . renderGame asset)
    eventHandler
    stepWorld

startingGS :: GameState
startingGS = findMoves $ GS{
      gspos=starting,
      lastClick=Nothing,
      promotion=Nothing,
      gsmoves=[],
      gsWhite=Nothing,
      gsBlack=Nothing
                   }

-- Plays the engine move if it is an engine's turn
stepWorld :: Float -> GameState -> IO GameState
stepWorld _ gs = do
  let pos = gspos gs
  let score = scoreGame pos (gsmoves gs)
  case score of
    Just res -> do
      putStrLn $ "game over: " ++ show res
      return startingGS
    Nothing -> do
      let toMove = posToMove pos
      let maybeEng = case toMove of
                  White -> gsWhite gs
                  Black -> gsBlack gs
      case maybeEng of
        Nothing -> return gs
        Just eng -> do
          putStrLn "engine Moving"
          mv <- eng pos
          putStrLn $ "engine chose: " ++ show mv
          return $ findMoves gs{gspos=doMove mv pos}

findMoves :: GameState -> GameState
findMoves gs = gs{gsmoves=legalMoves (gspos gs)}

renderGame :: BitmapData -> GameState -> Picture
renderGame bd gs@GS{promotion=Just _} = let
  mainBoard = renderGame bd gs{promotion=Nothing}
    in Pictures [mainBoard,promotionButtons bd (posToMove $ gspos gs)]
renderGame bd gs@GS{gspos=pos} = let
  boardRender = renderBoard bd (posBoard pos)
  marks = Pictures [markSpace s | s <- legalDestinations gs ]
    in Pictures [boardRender,marks]

eventHandler :: Event -> GameState -> IO GameState
eventHandler (EventKey (MouseButton LeftButton) Down _ pt) = handleClick pt
eventHandler _ = return

attemptMove :: Move -> GameState -> IO GameState
attemptMove mv gs = do
    let pos=gspos gs
    let toMove = posToMove pos
    let isHumanTurn = isNothing $ case toMove of
                                White -> gsWhite gs
                                Black -> gsBlack gs
    if not isHumanTurn
        then return gs
        else do
          putStrLn $ "interpretedMove as: " ++ show mv
          let legals = gsmoves gs
          if mv `elem` legals
             then do
                putStrLn "move is legal"
                putStrLn "playing move"
                print $ otherSide . posToMove . gspos $ gs
                print $ findPins pos
                let newPos = doMove mv pos
                return $ gs{
                  gspos=newPos,
                  lastClick=Nothing,
                  promotion=Nothing,
                  gsmoves= legalMoves newPos
                           }
              else do
                putStrLn "move is not legal"
                return $ gs{ lastClick=Nothing,promotion=Nothing }

handleClick :: Point -> GameState -> IO GameState
handleClick pt gs@GS{promotion=Just mv} = do
  let sp = clickToSpace pt
  if sp `notElem` [(9,i) | i <- [5..8] ]
      then return gs
      else do
        let promoteTo = case snd sp of
               5 -> Knight
               6 -> Bishop
               7 -> Rook
               8 -> Queen
               _ -> error "should be unreachable"
        let mv' = mv{becomes=promoteTo}
        attemptMove mv' gs
handleClick pt gs = do
  let lc = lastClick gs
  case lc of
      Nothing -> do
        let src = clickToSpace pt
        putStrLn $ "click at: " ++ showSpace src
        return gs{lastClick=Just src}
      Just src -> do
        let dest = clickToSpace pt
        let pos = gspos gs
        putStrLn $ "click at: " ++ showSpace dest
        putStrLn $ "interpreting: " ++ showSpace src ++ " -> " ++ showSpace dest
        let maybemv = interpretMove (src,dest) pos
        case maybemv of
            Nothing -> do
                putStrLn "failed to interpretMove"
                return gs{lastClick=Nothing}
            Just mv -> do
                case mv of
                  Promotion{} -> return $ gs{promotion=Just mv}
                  _           -> attemptMove mv gs


clickToSpace :: Point -> Space
clickToSpace (x,y) = (round$ x/100 + 4.5,round$ y/100 + 4.5)


--Interpret move does no legality or sensibility checking
--it just tries to be corect when the specified move is legal

interpretMove :: (Space,Space) -> Position -> Maybe Move
interpretMove (src,dest) pos = do
  let board = posBoard pos
  (_,pt) <- board!src
  let def = return Move{msrc=src,mdest=dest,mtaken=board!dest}
  case pt of
    Pawn -> if | fst src /= fst dest && isNothing (board!dest) -> return $ EnPasan  {msrc=src,mdest=dest}
               | snd dest == 8 || snd dest == 1                -> return $ Promotion{msrc=src,mdest=dest,mtaken=board!dest,becomes=undefined}
               | otherwise                                     -> def
    King -> case (fst src,fst dest) of
              (5,7) -> guard (snd src == snd dest) >> return (Castle K)
              (5,3) -> guard (snd src == snd dest) >> return (Castle Q)
              _     -> def
    _ -> def


legalDestinations :: GameState -> [Space]
legalDestinations GS{lastClick=Nothing} = []
legalDestinations GS{gspos=pos,lastClick=Just src,gsmoves=moves} = let
  fromSrc = filter (\mv -> startsAt pos mv == src) moves
  dests   = map (endsAt pos) fromSrc
    in dests

endsAt :: Position -> Move -> Space
endsAt pos (Castle kq) = case (posToMove pos,kq) of
                           (White,K) -> (7,1)
                           (White,Q) -> (3,1)
                           (Black,K) -> (7,8)
                           (Black,Q) -> (3,8)
endsAt _    mv = mdest mv

startsAt :: Position -> Move -> Space
startsAt pos Castle{} = case posToMove pos of
                          White -> (5,1)
                          Black -> (5,8)
startsAt _ mv = msrc mv
