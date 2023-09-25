module Models.Bids (PlayerBid(..)) where

import Models.Constants ( BidValue )
import Models.Players ( Player )

data PlayerBid = PlayerBid 
  { bidPlayer :: Player
  , bid       :: BidValue}
  deriving (Show)
