import Abalone
import Hex
import State
import View(ViewState, HexSize, hexSize, scene)
import Graphics.Element(Element)


type Input = WH WidthHeight | MP MousePosition | Click
inputs : Signal Input
inputs = mergeMany [map WH Window.dimensions, map Mousemove Mouse.position, map Click Mouse.clicks]

update : Input -> ViewState -> ViewState
update input (wh, position, (game, seg)) = case input of 
    (WH x) -> (x, position, abaloneState)
    (MP x) -> (wh, x, abaloneState)
    (Click) -> let clickedHex = xy2Pos wh game position 
                   t = (game, seg)
                   newAbaloneState = Maybe.withDefault t <| State.updateState t clickedHex
               in  (wh, position, newAbaloneState)

initialViewState = ((0,0), State.initial, (0,0))

viewState : Signal ViewState 
viewState = Signal.foldp update initialViewState inputs 

main : Signal Element
main = Signal.map scene viewState 

xy2Pos : WidthHeight -> Game -> (Int, Int) -> Maybe Hex.Position
xy2Pos (w,h) g (x,y) = 
    let size = hexSize (w,h) g
        xf = toFloat x
        yf = toFloat y 
        q = (x * sqrt(3)/3 - y / 3) / size
        r = y * 2/3 / size
        pos = Hex.hexRound (q,r)
    in  if Abalone.onBoard g.board pos then Just pos else Nothing



