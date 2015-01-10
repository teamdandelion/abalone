import Set
import Set(Set)
import List(map, (::))
import List
import Maybe
import Maybe(Maybe(..))

type alias Game = { board          : Board
                  , nextPlayer     : Player
                  , movesRemaining : Int
                  , marblesPerMove : Int
                  }

type alias Board = { whitePositions : Set Position
                   , blackPositions : Set Position
                   , boardRadius    : Int
                   }

type Direction = TopRight | MidRight | BotRight | TopLeft | MidLeft | BotLeft

type alias Move =  { segment   : Segment
                   , direction : Direction
                   } 

type alias Segment =  { basePos     : Position  
                      , orientation : Direction 
                      , segLength   : Int
                      , player      : Player   
                      }                 

type alias Position = (Int, Int)

type Player = White | Black 

getPieces : Board -> Player -> Set Position
getPieces b p = if p == White then b.whitePositions else b.blackPositions

start : Game 
start = Game standardBoard White 200 3

standardBoard : Board
standardBoard = let genPos d = (\x -> (-x, d))
                    spots = map (genPos 4) [0..4] ++ map (genPos 3) [-1..4] ++ [(-2, 2), (-1, 2), (0, 2)]
                    whitePos = Set.fromList spots
                    blackPos = Set.map (\(q, r) -> (-q, -r)) whitePos
                in
                    Board whitePos blackPos 5

dist2 : Position -> Position -> Int -- distance * 2 (to avoid fractional types)
dist2 (q1,r1) (q2,r2) = abs (q1 - q2) + abs (r1 - r2) + abs (q1 + r1 - q2 - r2)

adjacent : Direction -> Position -> Position
adjacent d (q, r) = case d of 
    TopRight -> (q+1, r-1)
    MidRight -> (q+1, r  )
    BotRight -> (q  , r+1)
    BotLeft  -> (q-1, r+1)
    MidLeft  -> (q-1, r  )
    TopLeft  -> (q  , r-1)

opposite : Direction -> Direction
opposite d = case d of 
    TopRight -> BotLeft
    MidRight -> MidLeft
    BotRight -> TopLeft
    BotLeft  -> TopRight
    MidLeft  -> MidRight
    TopLeft  -> BotRight

next : Player -> Player
next p = case p of 
    White -> Black
    Black -> White

colinear : Direction -> Direction -> Bool
colinear d1 d2 = d1 == d2 || d1 == opposite d2

inline : Move -> Bool
inline m = colinear (.direction m) (.orientation <| .segment <| m)

broadside : Move -> Bool
broadside m = not (inline m)

iterateN : Int -> (a -> a) -> a -> List a
iterateN len f x = if len == 0 then [] else x :: iterateN (len-1) f (f x)

segPieces : Segment -> List Position
segPieces {basePos, orientation, segLength} = iterateN segLength (adjacent orientation) basePos

gameOver : Game -> Bool
gameOver g = g.movesRemaining <= 0 || List.any (\p -> numPieces g p == 0) [White, Black]

winner : Game -> Maybe Player
winner g = let 
                advantage = case compare (numPieces g White) (numPieces g Black) of
                GT -> Just White
                LT -> Just Black
                EQ -> Nothing
           in
                if gameOver g then advantage else Nothing

numPieces : Game -> Player -> Int
numPieces g p = List.length <| Set.toList <| getPieces g.board p

-- this function will recieve new game states from client and verify validity
--isValid : Game -> Game -> Bool
--isValid g0 g1 = List.member g1 (futures g0) -- very inefficient impl but that should be fine since occurs once per turn

onBoard : Board -> Position -> Bool -- is a piece on the board still?
onBoard board pos = dist2 pos (0, 0) <= board.boardRadius * 2

-- ===
--futures : Game -> List Game -- find all valid future states of this board (1 move)
--futures g = map (update g) (possibleMoves g)

crossApply : List (a -> b) -> List a -> List b
crossApply fs xs = List.foldr (\a  -> (++) <| (flip List.map) xs a) [] fs
crossApply3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
crossApply3 f a b c = List.map f a `crossApply` b `crossApply` c

 --get every segment (distinct linear grouping) for current player in game
 --handle singletons seperately because otherwise they could be triple-counted
segments : Game -> List Segment
segments {board, nextPlayer, movesRemaining, marblesPerMove} = let 
    pieces : List Position
    pieces = Set.toList <| getPieces board nextPlayer
    segConstructor : Position -> Direction -> Int -> Segment
    segConstructor = (\p o l -> {basePos = p, orientation = o, segLength = l, player = nextPlayer})
    singletons : List Segment
    singletons = map (\p -> segConstructor p TopRight 1) pieces
    longer : List Segment
    longer = List.filter valid <| crossApply3 segConstructor pieces orients [2..marblesPerMove]
    orients = [TopRight, MidRight, BotRight]
    valid : Segment -> Bool
    valid = List.all (flip Set.member <| getPieces board nextPlayer) << segPieces
                            in singletons ++ longer

owner : Board -> Position -> Maybe Player
owner b x = if 
    | x `Set.member` getPieces b White -> Just White
    | x `Set.member` getPieces b Black -> Just Black
    | otherwise -> Nothing

inlineMoved : Board -> Move -> Maybe (List Position)
inlineMoved b m = if broadside m 
    then  Nothing
    else let front = if m.segment.orientation == m.direction then last else List.head
             attacked = (adjacent m.direction) << front <| segPieces (m.segment)
             clear x force = if  
                 | owner b x == Nothing -> Just []
                 | owner b x == Just (m.segment.player) || force == 0 -> Nothing
                 | otherwise -> Maybe.map ((::) x) <| clear (adjacent m.direction x) (force - 1)
         in clear attacked (m.segment.segLength - 1)


last : List a -> a
last = List.reverse >> List.head

isJust : Maybe a -> Bool
isJust x = if x == Nothing then False else True

valid : Game -> Move -> Bool
valid g m = let s = m.segment
                dir = m.direction
                b = g.board
                free x  = owner b x == Nothing
            in if broadside m 
                    then List.all free <| List.map (adjacent dir) (segPieces s)
                    else isJust <| inlineMoved b m


possibleMoves : Game -> List Move
possibleMoves g  = let moveConstructor = (\s d -> Move s d)
                       allDirections = [TopRight, MidRight, BotRight, TopLeft, MidLeft, BotLeft]
                       allMoves = crossApply (map moveConstructor <| segments g) allDirections
                   in List.filter (valid g) <| allMoves