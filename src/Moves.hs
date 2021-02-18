module Moves where

import Board
import Data.Array
import Data.Maybe
import Data.Tuple.Extra
import Control.Monad

data SlideType = BishopSlide | RookSlide deriving(Eq,Ord,Show)

legalMoves :: Position -> [Move]
legalMoves pos = let
  board = posBoard pos
  toMove = posToMove pos
  myPieces :: [(Space,PieceType)]
  myPieces = [ (space,snd$fromJust mPiece) |
                (space,mPiece) <- assocs board
              , isJust mPiece
              , fst (fromJust mPiece) == toMove
              ]
  pins = findPins pos
  free :: [(Space,PieceType)]
  free = filter (\(sp,_) -> sp `notElem` map fst3 pins) myPieces
  pined :: [((Space,PieceType),(Space,PieceType),SlideType)]
  pined = [ ((sp,fromJust $ lookup sp myPieces),(pinedBy,snd.fromJust$board!pinedBy),st) | (sp,pinedBy,st) <- pins ]
  castles :: [Move]
  castles = genCastles pos
  enpassan :: [Move]
  enpassan = genEnpassan pos
  moves = castles ++ enpassan ++ concatMap (pinedMoves pos) pined ++  concatMap (`legalMovesFor` pos) free
    in if isCheck toMove pos
          then filter (moveNotCheck pos) moves
          else moves

none :: Foldable t => (a -> Bool) -> t a -> Bool
none = fmap not . any

genCastles :: Position -> [Move]
genCastles pos = let
  side = posToMove pos
  board = posBoard pos
  crs = posCastlingRights pos
  kingPos = getKingPos side pos
  kr = crs!(side,K)
  qr = crs!(side,Q)
  rank = case side of
          White -> 1
          Black -> 8
  canKingSide = kr
                   && none isJust [board!(i,rank) | i <- [6,7] ]
                   && and [ moveNotCheck pos mv | mv <-
                      [ Move{msrc=kingPos,mdest=(6,rank),mtaken=Nothing}
                      , Castle{castleSide = K} ] ]
  canQueenSide = qr
                  && none isJust [ board!(i,rank) | i <- [2,3,4] ]
                  && and [ moveNotCheck pos mv | mv <-
                      [ Move{msrc=kingPos,mdest=(3,rank),mtaken=Nothing}
                      , Castle{castleSide = Q} ] ]

    in [ Castle K | canKingSide ] ++ [ Castle Q | canQueenSide ]


genEnpassan :: Position -> [Move]
genEnpassan pos = do
  mv <- maybeToList $ posLastMove pos
  (lmsrc,lmdest) <- case mv of
                  Move{msrc=src,mdest=dest} -> return (src,dest)
                  _ -> []
  let board = posBoard pos
  guard $ fmap snd (board!lmsrc) == Just Pawn
  let side = posToMove pos
  guard $ case otherSide side of
            White -> snd lmsrc == 2 && snd lmdest == 4
            Black -> snd lmsrc == 7 && snd lmdest == 5
  let lmFile = fst lmsrc
  file <- [lmFile+1,lmFile-1]
  guard $ inRange (1,8) file
  let rank = case side of
               White -> 5
               Black -> 4
  let src = (file,rank)
  guard $ board!src == Just (side,Pawn)
  return EnPasan{
    msrc  = src,
    mdest = (lmFile,case side of
                      White -> 6
                      Black -> 3)}

pinedMoves :: Position -> ((Space,PieceType),(Space,PieceType),SlideType) -> [Move]
pinedMoves pos ((s1,pt1),(s2,pt2),st) = let
      bishopCase = concatMap (movesFrom pos s1 . cleanSlide pos ) (slidePinResolver s2 (simpleBishopSlide s1))
      rookCase   = concatMap (movesFrom pos s1 . cleanSlide pos ) (slidePinResolver s2 (simpleRookSlide   s1))
                                in case (pt1,st) of
                                      (Knight,_)           -> []
                                      (Rook  ,BishopSlide) -> []
                                      (Bishop,BishopSlide) -> bishopCase
                                      (Queen ,BishopSlide) -> bishopCase
                                      (Rook  ,RookSlide)   -> rookCase
                                      (Queen ,RookSlide)   -> rookCase
                                      (Bishop,RookSlide)   -> []
                                      (Pawn  ,BishopSlide) -> let takeAtkr = Move{msrc=s1,mdest=s2,mtaken=Just (otherSide$posToMove pos ,pt2) } in [ takeAtkr | takeAtkr `elem` pawnTakes s1 pos ]
                                      (Pawn  ,RookSlide)   -> if fst s1 == fst s2 then pawnPushes s1 pos else []
                                      (King  ,_)           -> error "king pin"


slidePinResolver :: Space -> [[Space]] -> [[Space]]
slidePinResolver sp [r1,r2,r3,r4] = if  sp `elem` (r1 ++ r3)
                                       then [r1,r3]
                                       else [r2,r4]
slidePinResolver sp rays = error $ "bad list passed to slide pin resolver: " ++ show sp ++ " " ++ show rays
-- this function works because the rays are generated in sequence so the ods and evens are oposite




legalMovesFor :: (Space,PieceType) -> Position -> [Move]
legalMovesFor (sp,pt) = ( case pt of
  Pawn   -> pawnMoves
  Rook   -> rookMoves
  Knight -> knightMoves
  Bishop -> bishopMoves
  Queen  -> queenMoves
  King   -> kingMoves
    ) sp

type PieceMoves = Space -> Position -> [Move]

simpleRookSlide :: Space -> [[Space]]
simpleRookSlide (x,y) = [ [ (x,i) | i <- [y+1..8] ]         -- N
                        , [ (i,y) | i <- [x+1..8] ]         -- E
                        , [ (x,i) | i <- reverse [1..y-1] ] -- S
                        , [ (i,y) | i <- reverse [1..x-1] ] -- W
                        ]

simpleBishopSlide :: Space -> [[Space]]
simpleBishopSlide (x,y) = [ [ (x+i,y+i) | i <- [1..min (8-x) (8-y)] ] -- NE
                          , [ (x+i,y-i) | i <- [1..min (8-x) (y-1)] ] -- SE
                          , [ (x-i,y-i) | i <- [1..min (x-1) (y-1)] ] -- SW
                          , [ (x-i,y+i) | i <- [1..min (x-1) (8-y)] ] -- NW
                          ]

simpleQueenSlide :: Space -> [[Space]]
simpleQueenSlide = liftM2 (++) simpleRookSlide simpleBishopSlide

cleanSlide :: Position -> [Space] -> [Space]
cleanSlide _ [] = []
cleanSlide pos (sp:sps) = case posBoard pos ! sp of
                            Nothing -> sp:cleanSlide pos sps
                            Just (side,_) -> [ sp | posToMove pos /= side]


rookSlides :: PieceType -> Bool
rookSlides Rook  = True
rookSlides Queen = True
rookSlides _     = False

bishopSlides :: PieceType -> Bool
bishopSlides Bishop = True
bishopSlides Queen  = True
bishopSlides _      = False

slidesRight :: SlideType -> PieceType -> Bool
slidesRight RookSlide   = rookSlides
slidesRight BishopSlide = bishopSlides

findPins :: Position -> [(Space,Space,SlideType)]
findPins pos = let
  kingSp     = getKingPos (posToMove pos) pos
  board      = posBoard pos
  rookRays   = simpleRookSlide kingSp
  bishopRays = simpleBishopSlide kingSp
  rookPins   = [checkRayForPin RookSlide   (posToMove pos) [(sp,board!sp)| sp <- ray] | ray <- rookRays   ]
  bishopPins = [checkRayForPin BishopSlide (posToMove pos) [(sp,board!sp)| sp <- ray] | ray <- bishopRays ]
    in catMaybes $ rookPins ++ bishopPins

checkRayForPin :: SlideType -> Side -> [(Space,Square)] -> Maybe (Space,Space,SlideType)
checkRayForPin slideType side ray = let
  nonEmpties = take 2 $ filter (isJust . snd) ray
    in ( case nonEmpties of
         [(sp1,Just (c1,_)),(sp2,Just (c2,pt2))] -> if c1 == side && c2 /= side && slidesRight slideType pt2
                                              then Just (sp1,sp2,slideType)
                                              else Nothing
         _ -> Nothing
          )

pawnPushes :: PieceMoves
pawnPushes sp@(x,y) pos = let
  board = posBoard pos
  toMove = posToMove pos
  torpedoable = (toMove == White && y==2) || (toMove == Black && y==7)
  pushDir = if toMove == White then 1 else -1
  candPushes = (x,y+pushDir): [ (x,y+2*pushDir) | torpedoable ]
  pushes = takeWhile (\p -> null $ board!p) candPushes
    in movesFrom pos sp pushes

pawnTakes :: PieceMoves
pawnTakes sp@(x,y) pos = let
  board = posBoard pos
  toMove = posToMove pos
  pushDir = if toMove == White then 1 else -1
  candTakes = filter onBoard [(x+1,y+pushDir),(x-1,y+pushDir)]
  takes = [ p | p <- candTakes , isJust (board!p) && fst(fromJust (board!p)) /= toMove ]
    in movesFrom pos sp takes

pawnMoves :: PieceMoves
pawnMoves sp pos = let
  pushes = pawnPushes sp pos
  takes  = pawnTakes  sp pos
    in concatMap applyPromotions (pushes ++ takes)

--assumes the moved piece is a pawn don't map this over non-pawn moves
applyPromotions :: Move -> [Move]
applyPromotions mv@Move{msrc=src,mdest=dest@(_,rank),mtaken=taken} = if rank  == 8 || rank ==1
                                                                      then [ Promotion{msrc=src,mdest=dest,mtaken=taken,becomes=pt} | pt <- [Queen,Rook,Bishop,Knight] ]
                                                                      else return mv
applyPromotions _ = error "promotion applied inapropriately"

knightMoves :: PieceMoves
knightMoves sp pos = movesFrom pos sp $ [ dest | dest <- candidates , isNothing (board!dest) || fmap fst (board!dest) /= Just toMove ]
  where
    candidates = filter onBoard [ sp `offset` (sx*vx,sy*vy) | (vx,vy) <- [(1,2),(2,1)] , sx <- [-1,1] , sy <- [-1,1]  ]
    board = posBoard pos
    toMove = posToMove pos

offset :: Space -> Space -> Space
offset (x,y) (dx,dy) = (x+dx,y+dy)


rookMoves :: PieceMoves
rookMoves sp pos = movesFrom pos sp $ concatMap (cleanSlide pos) (simpleRookSlide sp)

bishopMoves :: PieceMoves
bishopMoves sp pos = movesFrom pos sp $ concatMap (cleanSlide pos) (simpleBishopSlide sp)

queenMoves :: PieceMoves
queenMoves sp pos = movesFrom pos sp $ concatMap (cleanSlide pos) (simpleQueenSlide sp)

kingMoves :: PieceMoves
kingMoves sp pos = filter (moveNotCheck pos) (kingMovesSimple sp pos)

kingMovesSimple :: PieceMoves
kingMovesSimple sp pos = let
  rough = filter onBoard [ sp `offset` (dx,dy) | dx <- [-1..1] , dy <- [-1..1] , (dx,dy) /= (0,0) ]
  board = posBoard pos
  final = [ dest | dest <- rough, isNothing (board!dest) || fmap fst (board!dest) /= fmap fst (board!sp) ]
    in movesFrom pos sp final

movesFrom :: Position -> Space -> [Space] -> [Move]
movesFrom pos sp sps = [  Move{msrc=sp,mdest=dest,mtaken=posBoard pos!dest} | dest <- sps ]

checkFrom :: Side -> Position -> PieceMoves -> (PieceType -> Bool) -> Bool
checkFrom kingSide pos movePattern predicate = let
  kingPos = getKingPos kingSide pos
  board   = posBoard pos
  threats = do
      mv <- movePattern kingPos pos
      case mv of
          Move _ dest _ -> do
            let sq = board!dest
            guard $ isJust sq
            let (side,pt) = fromJust sq
            guard $ side /= kingSide
            guard $ predicate pt
            return ()
          _ -> guard False
             in not.null$threats

isCheck :: Side -> Position -> Bool
isCheck kingSide pos = let
  pos' = pos{posToMove=kingSide}
  checkFrom'    = checkFrom kingSide pos'
  rookThreats   = checkFrom' rookMoves        rookSlides
  bishopThreats = checkFrom' bishopMoves      bishopSlides
  horseThreats  = checkFrom' knightMoves      (== Knight)
  pawnThreats   = checkFrom' pawnTakes        (== Pawn  )
  kingThreats   = checkFrom' kingMovesSimple  (== King  )
      in or [rookThreats,bishopThreats,horseThreats,pawnThreats,kingThreats]

otherSide :: Side -> Side
otherSide Black = White
otherSide White = Black

doMove :: Move -> Position -> Position
doMove mv@Move{msrc=src,mdest=dest} pos = let
  board  = posBoard pos
    in pos{
          posBoard= board // [(src,Nothing),(dest,board!src)]
          -- needs special cases for castling and on paassan
         ,posToMove = otherSide $ posToMove pos
         ,posCastlingRights = updateCastlingRights mv pos
         ,posLastMove = Just mv
         ,posHmc      = if fmap snd (board!src) == Just Pawn  || isJust (board!dest)
                          then 0
                          else succ (posHmc pos)

         ,posFmc       = if posToMove pos == Black then posFmc pos + 1 else posFmc pos
         ,posBlackKing = if posToMove pos /= Black then posBlackKing pos else if board!src == Just (Black,King) then dest else posBlackKing pos
         ,posWhiteKing = if posToMove pos /= White then posWhiteKing pos else if board!src == Just (White,King) then dest else posWhiteKing pos
          }
doMove mv@Castle{castleSide=kq} pos = let
                                        side = posToMove pos
                                      in ( case (side,kq) of
                                                (Black,K) -> doMove Move{msrc=(5,8),mdest=(7,8),mtaken=Nothing} pos{
                                                        posBoard=posBoard pos//[((8,8),Nothing),((6,8),Just (Black,Rook))] }
                                                (Black,Q) -> doMove Move{msrc=(5,8),mdest=(3,8),mtaken=Nothing} pos{
                                                        posBoard=posBoard pos//[((1,8),Nothing),((4,8),Just (Black,Rook))] }
                                                (White,K) -> doMove Move{msrc=(5,1),mdest=(7,1),mtaken=Nothing} pos{
                                                        posBoard=posBoard pos//[((8,1),Nothing),((6,1),Just (White,Rook))] }
                                                (White,Q) -> doMove Move{msrc=(5,1),mdest=(3,1),mtaken=Nothing} pos{
                                                        posBoard=posBoard pos//[((1,1),Nothing),((4,1),Just (White,Rook))] }
                                        ){
                                              posLastMove=Just mv,
                                              posCastlingRights=posCastlingRights pos //[((side,K),False),((side,Q),False)]
                                                                                                                     }
doMove mv@EnPasan{msrc=src@(_,sy),mdest=dest@(dx,_)} pos = (doMove Move{msrc=src,mdest=dest,mtaken=Nothing} pos{
                                              posBoard=posBoard pos//[((dx,sy),Nothing)]}){
                                              posLastMove=Just mv
                                                                                            }
doMove mv@Promotion{msrc=src,mdest=dest,mtaken=taken,becomes=pt} pos = let
  newPos = doMove Move{msrc=src,mdest=dest,mtaken=taken} pos
  newBoard = posBoard newPos
  promotedBoard = newBoard//[(dest,Just (posToMove pos,pt))]
    in newPos{
      posBoard=promotedBoard,
      posLastMove=Just mv
             }

updateCastlingRights :: Move -> Position -> Castling
updateCastlingRights Castle{} pos = let
  cr = posCastlingRights pos
  side = posToMove pos
    in cr // [((side,Q),False),((side,K),False)]
updateCastlingRights  Move{msrc=src,mdest=dest,mtaken=taken} pos = let
  board = posBoard pos
  moved = snd.fromJust $ board!src
  side  = posToMove pos
  lostByMove = case moved of
                  King -> if src == case side of
                                          White -> (5,1)
                                          Black -> (5,8)

                              then [((side,K),False),((side,Q),False)]
                              else []
                  Rook -> if src `elem` case side of
                                            White -> [(1,1),(8,1)]
                                            Black -> [(1,8),(8,8)]
                              then case fst src of
                                     1 -> [((side,Q),False)]
                                     8 -> [((side,K),False)]
                                     _ -> error "should be unreachable"
                              else []
                  _     -> []

  lostByCapture = case taken of
                     Nothing -> []
                     Just (_,pt) -> case pt of
                                      Rook -> if dest `elem` case side of
                                                    Black -> [(1,1),(1,8)]
                                                    White -> [(8,1),(8,8)]
                                                then case snd dest of
                                                      1 -> [((side,Q),False)]
                                                      8 -> [((side,K),False)]
                                                      _ -> error "should be unreachable"
                                                else []
                                      _    -> []
        in posCastlingRights pos//(lostByMove ++ lostByCapture)
updateCastlingRights EnPasan{} pos = posCastlingRights pos
updateCastlingRights Promotion{} pos = posCastlingRights pos -- while a promotion may capture a rook doMove for promotion calls the normal pawn move first which should correct the castling rights


moveNotCheck :: Position -> Move -> Bool
moveNotCheck pos move = not $ isCheck (posToMove pos) (doMove move pos)

getKingPos :: Side -> Position -> Space
getKingPos Black = posBlackKing
getKingPos White = posWhiteKing

onBoard :: Space -> Bool
onBoard = inRange ((1,1),(8,8))

