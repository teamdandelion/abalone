{-# LANGUAGE DeriveGeneric #-}

module Player (Player(White, Black), next) where

import Data.Aeson
import GHC.Generics

-- Player & Related Functions--
data Player = White | Black 
	deriving (Eq, Show, Read, Ord, Bounded, Enum, Generic)

instance FromJSON Player
instance ToJSON   Player


next :: Player -> Player
next White = Black
next Black = White