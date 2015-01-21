import Abalone(Game)
import Abalone
import Hex
import Hex(Position)
import State
import State(AbaloneState)
import View(ViewState, WidthHeight, MousePosition)
import View
import Misc
import Debug

import Maybe

import Signal
import Signal(Signal)
import Mouse
import Touch(Touch)
import Touch
import Window
import Graphics.Element(Element)

touchInteractionSignal : Signal (Maybe (Position, Position)) -- start and end of touch
touchInteractionSignal = Signal.map2 computeTouch Window.dimensions Touch.touches 

computeTouch : WidthHeight -> Maybe Touch -> Maybe (Position, Position)
computeTouch wh touch = if 
    | touch == Nothing -> Nothing
    | otherwise -> let start = View.xy2Pos wh game (touch.x0, touch.y0)
                       end   = View.xy2Pos wh game (touch.x , touch.y )
                   in  Just (start, end)


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

main : Signal Element
main = Signal.map View.scene viewState 



