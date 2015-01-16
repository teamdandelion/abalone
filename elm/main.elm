import Abalone(Game)
import Abalone
import Hex
import State
import View(ViewState, WidthHeight, MousePosition)
import View

import Maybe

import Signal
import Signal(Signal)
import Mouse
import Window
import Graphics.Element(Element)

type Input = WH WidthHeight | MP MousePosition | Click
inputs : Signal Input
inputs = Signal.mergeMany [Signal.map WH Window.dimensions
                          ,Signal.map MP Mouse.position
                          ,Signal.map (always Click) Mouse.clicks]

update : Input -> ViewState -> ViewState
update input (wh, (game, seg), position) = case input of 
    (WH x) -> (x, (game, seg), position)
    (MP x) -> (wh, (game, seg), x)
    (Click) -> let clickedHex = View.xy2Pos wh game position 
                   startingState = (game, seg)
                   maybeNewState : Maybe State.AbaloneState
                   maybeNewState = Maybe.map (State.updateState startingState) clickedHex
                   finalState = Maybe.withDefault startingState maybeNewState
               in  (wh, finalState, position)

initialViewState = ((500,500), State.initial, (0,0))

viewState : Signal ViewState 
viewState = Signal.foldp update initialViewState inputs 

main : Signal Element
main = Signal.map View.scene viewState 



