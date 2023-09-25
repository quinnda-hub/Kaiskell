module Game.Actions where

import Models.Cards ( Card )
import Models.Constants ( BidValue )

-- If MakeBid is Nothing then the player passes on bidding.
data GameAction = PlayCard Card |
                  MakeBid (Maybe BidValue)
