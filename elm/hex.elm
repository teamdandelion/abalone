module Hex where
import List
import Dict
import Misc
type Direction = TopRight | MidRight | BotRight | TopLeft | MidLeft | BotLeft

type alias Position = (Int, Int)

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

directions = [TopRight, MidRight, BotRight, BotLeft, MidLeft, TopLeft]

axial2cube : Position -> (Int, Int, Int)
axial2cube (q,r) = (q, -q-r, r)

cube2axial : (Int, Int, Int) -> Position
cube2axial (x,y,z) = (x,z)

-- Round two floating point coordinates to closest hexagon. Algorithim from RedBlobGames
hexRound : (Float, Float) -> Position
hexRound (q,r) = 
    let x = q
        y = -q-r
        z = r
        rx = round(x)
        ry = round(y)
        rz = round(z)
        xdiff = abs(toFloat rx - x)
        ydiff = abs(toFloat ry - y)
        zdiff = abs(toFloat rz - z)
        xAdj = if xdiff > ydiff && xdiff > zdiff then -ry-rz else rx
        yAdj = if ydiff > xdiff && ydiff > zdiff then -rx-rz else ry
        zAdj = if xdiff > ydiff && xdiff > zdiff then -rx-ry else rz
    in  cube2axial (xAdj,yAdj,zAdj)

-- Find the "close" directions; e.g. nearbyDirections MidRight = [TopRight, MidRight, BotRight]
nearbyDirections : Direction -> List Direction
nearbyDirections d = 
    let idx = Misc.fromJust <| Misc.index d directions
        nearby = List.map (\x -> (x + idx) % 6) [-1, 0, 1]
    in  List.map (Misc.fromJust << (flip Misc.retrieve <| directions)) nearby

colinear : Direction -> Direction -> Bool
colinear d1 d2 = d1 == d2 || d1 == opposite d2

hexagonalGrid : Int -> List Position
hexagonalGrid rad = List.concat <| List.map ring [0..rad - 1]

tailOrOnly : List a -> List a
tailOrOnly (x::xs) = if xs == [] then [x] else xs

ring : Int -> List Position
ring n = let ds = List.concat <| List.map (List.repeat n << adjacent) directions 
         in tailOrOnly <| List.scanl identity (-n, 0) ds 

-- if p1 and p2 are distinct and colinear, then give the direction of the line from p1 to p2
findDirection : Position -> Position -> Maybe Direction
findDirection (q1, r1) (q2, r2) = 
    if
        | q1 == q2 && r1 == r2 -> Nothing
        | q1 == q2             -> if r1 < r2 then Just BotRight else Just TopLeft
        | r1 == r2             -> if q1 < q2 then Just MidRight else Just MidLeft
        | r1 + q1 == r2 + q2   -> if q1 < q2 then Just TopRight else Just BotLeft
        | otherwise            -> Nothing