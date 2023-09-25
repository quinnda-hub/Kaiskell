module Models.Team (Team(..)) where

import Models.Players ( Player )

data Team = Team 
  { player1   :: Player
  , player2   :: Player 
  , teamScore :: Int } 
  deriving (Show, Eq)
