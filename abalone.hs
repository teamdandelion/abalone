module Abalone
  ( Game(Game)
  , Board(Board)
  , Player(White, Black)
  , Position
  , gameOver
  , winner
  , numPieces
  , futures
  , isValid
  , Direction
  , Segment
  , segments
  , (|>)
  , start
  ) where

import Data.Ord
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Applicative
import Control.Monad

data Game = Game { board          :: Board
                 , nextPlayer     :: Player
                 , movesRemaining :: Int
                 , marblesPerMove :: Int
                 } deriving (Eq, Show, Read)

data Board = Board { whitePositions :: Set Position
                   , blackPositions :: Set Position
                   , boardRadius    :: Int
                   } deriving (Eq, Show, Read)

getPieces :: Board -> Player -> Set Position
getPieces b White = whitePositions b
getPieces b Black = blackPositions b

start :: Game
start = Game standardBoard White 1000 3

standardBoard :: Board
standardBoard = Board whitePos blackPos 5
 where
  whitePos = Set.fromList $ [(-4,0,4),(-4,1,3),(-2,0,2)] >>= \(q,q',r) -> map (flip (,) r) [q..q']
  blackPos = Set.map (\(q, r) -> (-q, -r)) whitePos

-- Player & Related Functions--
data Player = White | Black
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

next :: Player -> Player
next White = Black
next Black = White

-- Position / Grid Functions --
type Position = (Int, Int)

dist2 :: Position -> Position -> Int -- distance * 2 (to avoid fractional types)
dist2 (q1,r1) (q2,r2) = abs (q1 - q2) + abs (r1 - r2) + abs (q1 + r1 - q2 - r2)

data Direction = TopRight | MidRight | BotRight
               | TopLeft  | MidLeft  | BotLeft  
  deriving (Eq, Show, Read, Ord, Bounded, Enum)

(|>) :: Position -> Direction -> Position
(q, r) |> TopRight = (q+1, r-1)
(q, r) |> MidRight = (q+1, r  )
(q, r) |> BotRight = (q  , r+1)
(q, r) |> BotLeft  = (q-1, r+1)
(q, r) |> MidLeft  = (q-1, r  )
(q, r) |> TopLeft  = (q  , r-1)

opposite :: Direction -> Direction
opposite TopRight = BotLeft
opposite MidRight = MidLeft
opposite BotRight = TopLeft
opposite BotLeft  = TopRight
opposite MidLeft  = MidRight
opposite TopLeft  = BotRight

colinear :: Direction -> Direction -> Bool
colinear d1 d2 = d1 == d2 || d1 == opposite d2

-- Moves are internal-only representation of a move - external API is just game states
data Move = Move { segment   :: Segment
                 , direction :: Direction
                 } deriving (Eq, Show, Read)

inline, broadside :: Move -> Bool
inline m@(Move s _) = colinear (direction m) (orientation s)
broadside m         = not (inline m)

-- A segment is a linear group of marbles that could move.
data Segment = Segment { basePos     :: Position  -- The start position of the segment
                       , orientation :: Direction -- The direction the segment grows in (irrelevant if len is 1)
                       , segLength   :: Int       -- The length of the segment
                       } deriving (Eq, Show, Read)

segPieces :: Segment -> [Position]
segPieces (Segment pos orient len) = take len $ iterate (|> orient) pos

gameOver :: Game -> Bool
gameOver g = movesRemaining g <= 0 || any (\p -> numPieces g p == 0) [White, Black]

winner :: Game -> Maybe Player
winner g | gameOver g = advantage
         | otherwise  = Nothing
 where
  advantage = case comparing (numPieces g) White Black of
    GT -> Just White
    LT -> Just Black
    EQ -> Nothing

numPieces :: Game -> Player -> Int
numPieces g p = Set.size $ getPieces (board g) p

-- this function will recieve new game states from client and verify validity
isValid :: Game -> Game -> Bool
isValid g0 g1 = g1 `elem` (futures g0) -- very inefficient impl but that should be fine since occurs once per turn

onBoard :: Board -> Position -> Bool -- is a piece on the board still?
onBoard board pos = dist2 pos (0, 0) <= (boardRadius board) * 2

update :: Game -> Move -> Game
update (Game b p remaining perMove) m@(Move s@(Segment pos orient len) dir) = newGame
 where
  -- Pieces to move
  ownPieces     = segPieces s
  enemyPieces
    | broadside m = []
    | inline m    = let start | orient == dir = last ownPieces |> dir
                              | orient /= dir = pos            |> dir
                     in unfoldr ( \(x, force) -> if not (enemy x) || force == 0
                                                 then Nothing
                                                 else Just (x, (x |> dir, force - 1)) )
                                (start, len - 1)
   where
    enemy x = x `Set.member` getPieces b (next p)

  -- New game state
  updated = filter (onBoard b) . map (|> dir)

  (whiteMoved, blackMoved) | p == White = (ownPieces, enemyPieces)
                           | p == Black = (enemyPieces, ownPieces)

  newWhitePos = (whitePositions b \\ whiteMoved) \/ updated whiteMoved
  newBlackPos = (blackPositions b \\ blackMoved) \/ updated blackMoved
  s \\ l = Set.difference s (Set.fromList l)
  s \/ l = Set.union      s (Set.fromList l)
 
  newBoard = Board newWhitePos newBlackPos (boardRadius b)
  newGame  = Game newBoard (next p) (remaining - 1) perMove

futures :: Game -> [Game] -- find all valid future states of this board (1 move)
futures g = map (update g) (possibleMoves g)

 {- Algorithm:
    - find all segments (distinct groupings that can move)
    - take cartesian product with all directions
    - see if that direction is a valid move for given segment
    - if orientation is aligned with direction, attempt a "forward" move - might push off
      opponent so more complex computation
    - if orientation is not aligned with direction, attempt a "broadside" - just check
      that all destination spaces are free
 -}
possibleMoves :: Game -> [Move]
possibleMoves g@(Game b p _ _)  = do
  move <- Move <$> segments g <*> [TopRight .. BotLeft]
  guard $ valid move
  return move
 where
  valid m@(Move s@(Segment pos orient len) dir)
    | broadside m = all free $ map (|> dir) (segPieces s)
    | inline m    = let start | orient == dir = last (segPieces s) |> dir
                              | orient /= dir = pos                |> dir
                        clear x force | free x              = True
                                      | own x || force == 0 = False
                                      | otherwise           = clear (x |> dir) (force - 1)
                     in clear start (len - 1)
   where
    own   x = x `Set.member` getPieces b p
    enemy x = x `Set.member` getPieces b (next p)
    free  x = not (own x || enemy x)

-- get every segment (distinct linear grouping) for current player in game
-- handle singletons seperately because otherwise they could be triple-counted
segments :: Game -> [Segment]
segments (Game b p _ maxlen) = singletons ++ lengthTwoOrMore
 where
  singletons = do
    pos <- Set.toList $ getPieces b p
    return $ Segment pos TopRight 1
  lengthTwoOrMore = do
    pos    <- Set.toList $ getPieces b p
    orient <- [TopRight, MidRight, BotRight]
    len    <- [2..maxlen]
    let seg = Segment pos orient len
    guard $ valid seg
    return seg
   where
    valid = all (`Set.member` getPieces b p) . segPieces
