import Abalone(Game)
import Abalone
import Hex
import Hex(Position)
import State
import State(AbaloneState)
import View(ViewState, WidthHeight, MousePosition)
import View
import Misc
import Misc(fromJust, isJust)
import Debug
import Time(every)

import Maybe

import Signal
import Signal(Signal)
import Mouse
import Touch(Touch)
import Touch
import Window
import Graphics.Element(Element)
import Http(Request, get, post)

touchInteractionSignal : Signal (Maybe (Position, Position)) -- start and end of touch
touchInteractionSignal = Signal.map2 computeTouch Window.dimensions Touch.touches 

hoverSignal : Signal (Maybe Position)
hoverSignal = Signal.map (View.xy2pos 5) Window.dimensions Mouse.position

computeTouch : WidthHeight -> Maybe Touch -> Maybe (Position, Position)
computeTouch wh touch = if 
    | touch == Nothing -> Nothing
    | otherwise -> let start = View.xy2Pos wh game (touch.x0, touch.y0)
                       end   = View.xy2Pos wh game (touch.x , touch.y )
                   in  if (isJust start && isJust end) 
                            then Just (fromJust start, fromJust end)
                            else Nothing 



update : Maybe (Position, Position) -> AbaloneState -> AbaloneState
update touch (game, seg) = case touch of 
    Nothing -> (game, seg)
    Just (start, end) -> if 
        | start `elem` State.moves (game, seg)
            -> State.moveState (game, seg) start
        | start `elem` getPieces game.board game.nextPlayer 
            -> (game, Abalone.selectSegment game start end)
        | otherwise -> (game, seg)

gameState : Signal AbaloneState
gameState = Signal.foldp update State.initial touchInteractionSignal

statesNeedingPlayerMove : Signal AbaloneState 

statesNeedingServerMove : Signal AbaloneState 


playerMove : Maybe (Position, Position) -> AbaloneState -> AbaloneState 

serverMove : AbaloneState -> AbaloneState 




poll : Request String 
poll = get "localhost:8999"

pollResults : Signal (Request String)
pollResults = Signal.map (always poll) (every 500)

main : Signal Element
main = Signal.map View.scene viewState






