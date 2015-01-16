module State 
    ( AbaloneState
    , extensions
    , reductions
    , moves
    , updateState
    , initial
    ) where 

{-- 
    State is very simple:
        The game state, represented as an Abalone.Game
        The currently selected Segment (if any). Could be a single stone, or 
            a sequence of stones.

    It is modified in the following ways:
        Extension - Grow the segment to include more stones
        Reduction - take existing segment and make it smaller by removing an end stone
        Move - Commit the current move and switch control to opposing player

    The UI (encoded mostly in View.elm) is very simple; all input is handled via the user
        clicking on a hex position. The clicks can be extending/reducing/moving as per above.

    Each state transition function has an exported function that enumerates all valid hexes
        that will cause that transition, to provide feedback to the user.
--}

import Abalone(Game, Segment, Move)
import Abalone
import Hex
import Maybe
import List
import Misc
import Misc(fromJust)
import Set
import Set(Set)

type alias AbaloneState = (Game, Maybe Segment)

extensions : AbaloneState -> Set Hex.Position
extensions (g, seg) = if 
    | seg == Nothing -> Abalone.getPieces g.board g.nextPlayer
    | (fromJust seg).segLength >= g.marblesPerMove -> Set.empty
    | (fromJust seg).segLength == 1 ->
        let spaces = List.map (\x -> Hex.adjacent x (fromJust seg).basePos) Hex.directions
            pieces = Abalone.getPieces g.board g.nextPlayer
        in Set.fromList <| List.filter (\x -> x `Set.member` pieces) spaces
    | otherwise -> 
        let s = fromJust seg 
            orient = fromJust s.orientation
            behind = Hex.adjacent (Hex.opposite orient) s.basePos
            forward = Hex.adjacent orient (Misc.last <| Abalone.segPieces s)
            pieces = Abalone.getPieces g.board g.nextPlayer
        in  Set.fromList <| List.filter (\x -> x `Set.member` pieces) [behind, forward]


-- Take a game and partially built segment and extend it (if possible)
extendSegment : AbaloneState -> Hex.Position -> Maybe Segment
extendSegment (g,s) pos = if 
    | not <| pos `Set.member` extensions (g,s) -> Nothing
    | s == Nothing -> Just {basePos = pos, orientation = Nothing, segLength = 1, player = g.nextPlayer}
    | otherwise      -> let seg = fromJust s
                        in  Just {seg | basePos     <- pos
                                      , orientation <- Hex.findDirection pos seg.basePos
                                      , segLength   <- seg.segLength + 1}

extendState : AbaloneState -> Hex.Position -> Maybe AbaloneState
extendState (g,s) p = 
    let maybeSeg = extendSegment (g,s) p
        segToState = (\x -> (g, Just x))
    in  Maybe.map segToState maybeSeg

-- return a set containing the front and end of a segment
extrema : Segment -> Set Hex.Position
extrema s = Set.fromList <| Misc.apply [List.head, Misc.last] <| Abalone.segPieces s

reductions : AbaloneState -> Set Hex.Position
reductions (g, s) = Maybe.withDefault Set.empty <| Maybe.map extrema s

reduceState : AbaloneState -> Hex.Position -> Maybe AbaloneState
reduceState (g, s) p = if 
    | s == Nothing -> Nothing 
    | p /= (fromJust s).basePos && p /= (fromJust >> Abalone.segPieces >> Misc.last) s -> Nothing
    | (fromJust >> .segLength) s == 1 -> Just (g, Nothing)
    | otherwise ->  
        let seg = fromJust s 
            base = seg.basePos
            end = Misc.last <| Abalone.segPieces seg
            orient = fromJust seg.orientation
            newStart = if p == base then Hex.adjacent orient base else base
            newSeg = {seg | basePos <- newStart, segLength <- seg.segLength - 1}
        in  Just (g, Just newSeg)


{--
    The way the UI handles moves is as follows:
    The user has chosen a segment. Now, there are (at most) 6 possible directions the segment could move in. 
    So we will desginate six hex segments as representing the directions the segment can move into - this simplifies
    the UI since it allows us to only consider clicks on hexes rather than having a UI where the input interpretation
    is more conextual.

    To choose the six 'move-generating' tiles, we take the (arbitrarily) "front" end of the segment and designate the cells 
    to the left, forward, and right of the "front" as representing moves in that direction. We treat the "back" symmetrically.

--}


moves : AbaloneState -> Set Hex.Position
moves (g,s) = if 
    | s == Nothing -> Set.empty
    | otherwise -> let validMove (p,d) = Abalone.valid g <| fromJust <| generateMove (g,s) p
                       possibilities = adjacentHexDirs <| fromJust s
                   in  Set.fromList <| List.map fst <| List.filter validMove possibilities


generateMove : AbaloneState -> Hex.Position -> Maybe Move
generateMove (g,s) p = if 
    | s == Nothing -> Nothing
    | otherwise -> let possibilities = adjacentHexDirs <| fromJust s
                       direction = Maybe.map snd <| Misc.find (\x -> fst x == p) possibilities
                    in Maybe.map (\d -> {segment = fromJust s, direction = d}) direction


vanguard : Hex.Position -> Hex.Direction -> List (Hex.Position, Hex.Direction)
vanguard p d = List.map (\d -> (Hex.adjacent d p, d)) <| Hex.nearbyDirections d

adjacentHexDirs : Segment -> List (Hex.Position, Hex.Direction)
adjacentHexDirs seg = if 
    | seg.orientation == Nothing -> List.map (\d -> (Hex.adjacent d seg.basePos, d)) Hex.directions
    | otherwise -> let o = fromJust seg.orientation
                       pieces = Abalone.segPieces seg
                       front = vanguard (List.head pieces) (Hex.opposite o)
                       back  = vanguard (Misc.last pieces) o 
                    in front ++ back

moveState : AbaloneState -> Hex.Position -> Maybe AbaloneState
moveState (g,s) p = 
    let uncheckedMove = generateMove (g,s) p 
        validator : Move -> Maybe Move
        validator = Misc.validate (Abalone.valid g)
        checkedMove = Maybe.andThen uncheckedMove validator
    in  Maybe.map (\m -> (Abalone.update g m, Nothing)) checkedMove

initial : AbaloneState 
initial = (Abalone.start, Nothing)

updateState : AbaloneState -> Hex.Position -> AbaloneState
updateState t p = Maybe.withDefault t <| Maybe.oneOf [reduceState t p, extendState t p, moveState t p]


