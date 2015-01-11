module Player where
type Player = White | Black 

next : Player -> Player
next p = case p of 
    White -> Black
    Black -> White
