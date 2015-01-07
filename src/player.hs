module Player (Player(White, Black), next) where
-- Player & Related Functions--
data Player = White | Black deriving (Eq, Show, Read, Ord, Bounded, Enum)
next :: Player -> Player
next p = case p of
	White -> Black
	Black -> White