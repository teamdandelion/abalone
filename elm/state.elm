module State where 
import Abalone(..)
import Hex
import Maybe
import List
import List(map)
import Misc
import Misc(fromJust)
import Set
import Set(Set)

type alias AbaloneState = (Game, Maybe Segment)
type alias Update = Hex.Position -> AbaloneState -> Maybe AbaloneState

{-- 
    Input right now is a very simple model: The user selects a Segment (a movable line of
    abalone stones that they control, of length at most marblesPerMove) and may select a 
    direction to move them in. 

    The State is thus a combination of:
        - the present Abalone.Game 
        - the presently selected Segment
--}


extensions : Game -> Maybe Segment -> Set Hex.Position
extensions g seg = if 
    | seg == Nothing -> getPieces g.board g.nextPlayer
    | (fromJust seg).segLength >= g.marblesPerMove -> Set.empty
    | (fromJust seg).segLength == 1 ->
        let spaces = map (\x -> Hex.adjacent x (fromJust seg).basePos) Hex.directions
            pieces = getPieces g.board g.nextPlayer
        in Set.fromList <| List.filter (\x -> x `Set.member` pieces) spaces
    | otherwise -> 
        let s = fromJust seg 
            orient = fromJust s.orientation
            behind = Hex.adjacent (Hex.opposite orient) s.basePos
            forward = Hex.adjacent orient (Misc.last <| segPieces s)
            pieces = getPieces g.board g.nextPlayer
        in  Set.fromList <| List.filter (\x -> x `Set.member` pieces) [behind, forward]


-- Take a game and partially built segment and extend it (if possible)
extendSegment : AbaloneState -> Hex.Position -> Maybe Segment
extendSegment (g,s) pos = if 
    | not <| pos `Set.member` extensions g s -> Nothing
    | s == Nothing -> Just {basePos = pos, orientation = Nothing, segLength = 1, player = g.nextPlayer}
    | otherwise      -> let seg = fromJust s
                        in  Just {seg | basePos     <- pos
                                      , orientation <- Hex.findDirection pos seg.basePos
                                      , segLength   <- seg.segLength + 1}

reduceState : AbaloneState -> Hex.Position -> Maybe AbaloneState
reduceState (g, s) p = if 
    | s == Nothing -> Nothing
    | otherwise -> 
        let seg = fromJust s 
        in if 
            | seg.segLength == 1 && p == seg.basePos -> Just (g, Nothing)
            | p == seg.basePos -> Just (g, Just {seg | basePos   <- Hex.adjacent (fromJust seg.orientation) seg.basePos
                                                     , segLength <- seg.segLength - 1})
            | p == (Misc.last <| segPieces seg) -> Just (g, Just {seg | segLength <- seg.segLength - 1})
            | otherwise -> Nothing

generateMove : AbaloneState -> Hex.Position -> Maybe Move
generateMove (g, s) p = if
    | s == Nothing -> Nothing
    | otherwise -> 
        let seg = fromJust s
            oppositeOrientation = Maybe.map Hex.opposite seg.orientation
            directionFromBase = generateMoveHelper seg.basePos oppositeOrientation p
            endPos = Misc.last <| segPieces seg
            directionFromEnd  = generateMoveHelper endPos seg.orientation p
            direction = Maybe.oneOf [directionFromBase, directionFromEnd]
        in Maybe.map (\d -> {segment = seg, direction = d}) direction


generateMoveHelper : Hex.Position -> Maybe Hex.Direction -> Hex.Position -> Maybe Hex.Direction
generateMoveHelper segmentEnd segmentDirection proposedPosition = if 
    | Hex.dist2 segmentEnd proposedPosition /= 2 -> Nothing
    | segmentDirection == Nothing -> Hex.findDirection segmentEnd proposedPosition
    | otherwise -> 
        let proposedDirection = Hex.findDirection segmentEnd proposedPosition
        in  if (fromJust proposedDirection) `List.member` Hex.nearbyDirections (fromJust segmentDirection)
                then proposedDirection
                else Nothing

moveState : AbaloneState -> Hex.Position -> Maybe AbaloneState
moveState (g,s) p = 
    let uncheckedMove = generateMove (g,s) p 
        checkedMove = Maybe.andThen uncheckedMove (\m -> if valid g m then Just m else Nothing)
    in  Maybe.map (\m -> (update g m, Nothing)) checkedMove

initial : AbaloneState 
initial = (start, Nothing)

updateState : AbaloneState -> Hex.Position -> AbaloneState
updateState t p = Maybe.withDefault t <| Maybe.oneOf [reduceState t p, extendState t p, moveState t p]

extendState : AbaloneState -> Hex.Position -> Maybe AbaloneState
extendState (g,s) p = 
    let maybeSeg = extendSegment (g,s) p
        segToState = (\x -> (g, Just x))
    in  Maybe.map segToState maybeSeg
