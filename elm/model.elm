import Set
import Set(Set)
import List(map, (::))
import List

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

colinear : Direction -> Direction -> Bool
colinear d1 d2 = d1 == d2 || d1 == opposite d2

inline : Move -> Bool
inline m = colinear (.direction m) (.orientation <| .segment <| m)

broadside : Move -> Bool
broadside m = not (inline m)

iterateN : Int -> (a -> a) -> a -> List a
iterateN len f x = if len == 0 then [] else x :: iterateN (len-1) f (f x)

segPieces : Segment -> List Position
segPieces {pos, orient, len} = iterateN len (adjacent orient) pos

gameOver : Game -> Bool
gameOver g = g.movesRemaining <= 0 || List.any (\p -> numPieces g p == 0) [White, Black]

winner : Game -> Maybe Player
winner g = let 
                advantage = case compare (numPieces g) White Black of
                GT -> Just White
                LT -> Just Black
                EQ -> Nothing
           in
                if gameOver g then advantage else Nothing

numPieces : Game -> Player -> Int
numPieces g p = List.length <| Set.toList <| getPieces g.board p

-- this function will recieve new game states from client and verify validity
isValid : Game -> Game -> Bool
isValid g0 g1 = List.member g1 (futures g0) -- very inefficient impl but that should be fine since occurs once per turn

onBoard : Board -> Position -> Bool -- is a piece on the board still?
onBoard board pos = dist2 pos (0, 0) <= board.boardRadius * 2

-- ===
futures : Game -> List Game -- find all valid future states of this board (1 move)
futures g = map (update g) (possibleMoves g)

