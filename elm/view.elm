module View where
import Set
import List(map)
import List
import Maybe

import Color 
import Color(Color)
import Graphics.Collage as Collage
import Graphics.Element(Element)
import Graphics.Collage(Shape, Form)
import Graphics.Element as Element
import Text

import Abalone 
import Hex
import Player
import Player(Player(..))
import Misc
import State(AbaloneState)
import State

type alias WidthHeight = (Int, Int)
type alias HexSize = Float
type alias MousePosition = (Int, Int)

type alias ViewState = (WidthHeight, AbaloneState, MousePosition)

niceCollage (w,h) = Collage.collage w h 

xy2Pos : WidthHeight -> Abalone.Game -> (Int, Int) -> Maybe Hex.Position
xy2Pos (w,h) g (x,y) = 
    let size = hexSize (w,h) g
        xf = toFloat x - toFloat w / 2
        yf = -(toFloat y - toFloat h / 2)
        q = (xf * sqrt(3)/3 - yf / 3) / size
        r = yf * 2/3 / size
        pos = Hex.hexRound (q,r)
    in  if Abalone.onBoard g.board pos then Just pos else Nothing

scene : ViewState -> Element
scene vs = 
    let 
        (wh, (game, segment), xy) = vs
        backgroundBoard = drawBoard vs 
        marbles = stones wh game
        highlights = highlighter wh (game, segment) 
        selectedPos = xy2Pos wh game xy
    in  Element.layers [backgroundBoard, highlights, marbles]

type CellState = Regular | Extension | Reduction | Move 

state2Color : (CellState, Bool) -> Color
state2Color (state, hovered) = if 
    | state == Regular   -> if hovered then Color.darkGrey   else Color.grey
    | state == Extension -> if hovered then Color.green  else Color.lightGreen
    | state == Reduction -> if hovered then Color.purple    else Color.lightPurple
    | state == Move      -> if hovered then Color.orange else Color.lightOrange

drawBoard : ViewState -> Element
drawBoard (wh, (g,s), xy) = 
    let 
        size = hexSize wh g
        cells = Hex.hexagonalGrid g.board.boardRadius
        hoveredCell = xy2Pos wh g xy
        states = map (getState (g,s)) cells
        hovered = map (\x -> Just x == hoveredCell) cells
        colors = map state2Color <| Misc.zip states hovered
        cellements = map (drawHex size) <| Misc.zip cells colors
    in  niceCollage wh cellements

getState : AbaloneState -> Hex.Position -> CellState
getState t = 
    let
        extensions = State.extensions t
        reductions = State.reductions t 
        moves = State.moves t 
    in  (\p -> if 
        | p `Set.member` extensions -> Extension
        | p `Set.member` reductions -> Reduction
        | p `Set.member` moves -> Move
        | otherwise -> Regular)

highlighter : WidthHeight -> AbaloneState -> Element 
highlighter wh (g,s) = 
    let pieces = Maybe.withDefault [] <| Maybe.map Abalone.segPieces s
        size = hexSize wh g
        circle p = Collage.circle (size * 0.7) |> Collage.filled Color.green |> reposition size p
    in  niceCollage wh <| map circle pieces

hexagon : HexSize -> (Shape -> Form) -> Form
hexagon size style = Collage.ngon 6 size |> style |> Collage.rotate (degrees 30)

drawHex : HexSize -> (Hex.Position, Color) -> Form 
drawHex size (pos, color) = let 
        outliner = Collage.outlined <| Collage.solid Color.black
        filler = Collage.filled color
        group = Collage.group <| map (hexagon size) [outliner, filler]
    in reposition size pos group
    
reposition : HexSize -> Hex.Position -> Form -> Form
reposition pixelRadius (q, r) hex = 
    let qf = toFloat q
        rf = toFloat r
        x = pixelRadius * sqrt(3) * (qf + rf/2)
        y = pixelRadius * 3/2 * rf
    in  Collage.move (x, y) hex

hexSize : WidthHeight -> Abalone.Game -> HexSize
hexSize (w, h) game = let wholeBoardLen = toFloat (min w h)
                          hexesOnEdge = game.board.boardRadius
          in wholeBoardLen / (toFloat <| hexesOnEdge * 4) -- discovered experimentally ;)

stone : HexSize -> Player -> Hex.Position -> Form
stone size p pos = Collage.circle (size / 2) |> Collage.filled (Player.colorOf p) |> reposition size pos

stones : WidthHeight -> Abalone.Game -> Element
stones wh game = 
    let size = hexSize wh game
        drawPlayerStones player = player |> Abalone.getPieces game.board >> Set.toList >> map (stone size player)
        f = (\player -> map (stone size player) <| Set.toList <| Abalone.getPieces game.board player)
    in  [White, Black] |> map drawPlayerStones >> List.concat >> niceCollage wh 


















