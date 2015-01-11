import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Signal
import Window
import Touch
import Hex
import Misc
import List(map)
import Text

import Debug

radiusInHex = 5

main : Signal Element
main = Signal.map2 scene Window.dimensions Touch.taps

scene : (Int, Int) -> {x:Int, y:Int } -> Element
scene (w,h) {x,y} = 
    let positioned = move (toFloat x - toFloat w/2, toFloat h/2 - toFloat y)
        taps = collage w h [positioned (filled purple (circle 40)) ]
    in 
        layers [taps, board radiusInHex (w, h)]

margin = 0

hexagon : Float -> Shape
hexagon = ngon 6

reposition : Float -> Form -> Hex.Position -> Form
reposition pixelRadius hex (q, r) = let 
    qf = toFloat q
    rf = toFloat r
    x = pixelRadius * sqrt(3) * (qf + rf/2)
    y = pixelRadius * 3/2 * rf
                                    in move (x, y) hex

hexSize : (Int, Int) -> Int -> Float
hexSize (w, h) hexesOnEdge = let wholeBoardLen = toFloat (min w h - margin)
          in wholeBoardLen / (toFloat <| hexesOnEdge * 4) -- discovered experimentally ;)

genHex : Float -> Hex.Position -> Form
genHex size pos = let 
    style = outlined <| solid black
    hex = rotate (degrees 30) <| style <| hexagon size 
    coord = toForm <| Text.plainText <| toString pos
    gp : Form 
    gp = group [hex, coord]
         in reposition size gp pos 

board : Int -> (Int, Int) -> Element
board hexesOnEdge (w, h) = let 
    size = hexSize (w, h) hexesOnEdge
    hexPositions : List Hex.Position
    hexPositions = Debug.log "hex positions" <| Hex.hexagonalGrid hexesOnEdge
    hexagons : List Form
    hexagons = map (genHex size) hexPositions
                         in collage w h hexagons