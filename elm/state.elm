import Abalone(..)
import Hex
import Maybe
import List
import List(map)
import Misc
import Set
import Set(Set)

type alias State = (Game, Maybe Segment)
type alias Update = Hex.Position -> State -> Maybe State

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
    | seg == Nothing -> getPieces (g.board g.nextPlayer)
    | (Misc.fromJust seg).segLength >= g.marblesPerMove -> Set.empty
    | otherwise -> 
        let s = Misc.fromJust seg 
            dirAndOpposite = (\d -> [d, Hex.opposite d])
            directions = Maybe.withDefault Hex.directions <| Maybe.map dirAndOpposite seg.orientation
            possibleSpaces = map (\x -> Hex.adjacent x seg.basePos) directions
            pieces = getPieces g.board g.nextPlayer
        in  List.filter (\x -> x `Set.member` pieces) possibleSpaces


-- Take a game and partially built segment and extend it (if possible)
extendSegment : State -> Hex.Position -> Maybe Segment
extendSegment (g,s) pos = if 
    | not <| pos `Set.member` extensions g s -> Nothing
    | s == Nothing -> Just {basePos = pos, orientation = Nothing, segLength = 1, player = g.nextPlayer}
    | otherwise      -> let seg = Misc.fromJust s
                        in  Just {seg | basePos     <- pos
                                      , orientation <- Hex.findDirection pos seg.basePos
                                      , segLength   <- seg.segLength + 1}

reduceState : State -> Hex.Position -> Maybe State
reduceState (g, s) p = if 
    | s == Nothing -> Nothing
    | otherwise -> 
        let seg = Misc.fromJust s 
        in if 
            | seg.segLength == 1 && p == seg.basePos -> Just (g, Nothing)
            | p == seg.basePos -> Just (g, {seg | basePos   <- Hex.adjacent seg.orientation seg.basePos
                                                , segLength <- seg.segLength - 1})
            | p == Misc.last <| segPieces seg -> Just (g, {seg | segLength <- seg.segLength - 1})
            | otherwise -> Nothing

generateMove : State -> Hex.Position -> Maybe Move
generateMove (g, s) p = 
    let directionFromBase = generateMoveHelper s.basePos (Hex.opposite s.orientation) p
        endPos = Misc.last <| segPieces s 
        directionFromEnd  = generateMoveHelper endPos s.orientation p
        direction = Maybe.oneOf [directionFromBase, directionFromEnd]
    in Maybe.map (\d -> {segment = s, direction = d}) direction


generateMoveHelper : Hex.Position -> Hex.Direction -> Hex.Position -> Maybe Hex.Direction
generateMoveHelper segmentEnd segmentDirection proposedPosition = if 
    | Hex.dist2 segmentEnd proposedPosition /= 2 -> Nothing
    | segmentDirection == Nothing -> Hex.findDirection segmentEnd proposedPosition
    | otherwise -> 
        let proposedDirection = Hex.findDirection segmentEnd proposedPosition
        in  if proposedDirection `List.member` Hex.nearbyDirections segmentDirection
                then Just proposedDirection
                else Nothing

moveState : State -> Hex.Position -> Maybe State
moveState (g,s) p = 
    let uncheckedMove = generateMove (g,s) p 
        checkedMove = Maybe.map (\m -> if valid g m then Just m else Nothing) uncheckedMove
    in  Maybe.map (\m -> (update g m, Nothing)) checkedMove

initialState : State 
initialState = (start, Nothing)

updateState : State -> Hex.Position -> State
updateState t p = Maybe.withDefault t <| Maybe.oneOf [reduceState t p, extendState t p, moveState t p]

--reduceSegment : State -> Hex.Position -> Maybe State
--reduceSegment (g,s) p =  Maybe.map (\s -> (g,s)) <| Abalone.reduceSegment g s p

extendState : State -> Hex.Position -> Maybe State
extendState (g,s) p = 
    let maybeSeg = extendSegment (g,s) p
        segToState = (\x -> (g, Just x))
    in  Maybe.map segToState maybeSeg

--updateGame : State -> Hex.Position -> Maybe State
--updateGame (g,s) p = Maybe.map (\m -> (Abalone.update g m)) <| Abalone.constructMove g s p 
