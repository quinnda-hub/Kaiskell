module Models.Players (Player(..)) where

import Models.Cards (Card)

data Player = Player 
  { playerName :: String
  , hand       :: [Card] }
  deriving (Show, Eq)
