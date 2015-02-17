module Player where
import Color
type Player = White | Black 

next : Player -> Player
next p = case p of 
    White -> Black
    Black -> White

colorOf : Player -> Color.Color
colorOf p = case p of 
    White -> Color.white
    Black -> Color.black