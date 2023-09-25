module Models.State where

import Data.Map ( Map )
import Models.Bids ( PlayerBid )
import Models.Cards ( Card, Deck, Suit )
import Models.Constants (BidValue)
import Models.Players ( Player )
import Models.Team ( Team )
import System.Random ( StdGen )

data CardPlay = CardPlay 
  { cardPlayer :: Player
  , playedCard :: Card}
  deriving (Show, Eq)

data Trick = Trick 
  { plays  :: [CardPlay]
  , winner :: Maybe Player}
  deriving (Show)

data RoundInfo = RoundInfo 
  { allBids   :: [PlayerBid]
  , allCards  :: [CardPlay]
  , allTricks :: [Trick]} 
  deriving (Show)

-- A trump is just a Suit.
-- If Trump is Nothing, that indicates there is no Trump.
type Trump = Maybe Suit

newtype GameHistory = GameHistory (Map Int RoundInfo)
  deriving (Show)

data GameState = GameState
 { gen                  :: StdGen 
 , team1                :: Team
 , team2                :: Team
 , currentDeck          :: Deck
 , currentPlayer        :: Player
 , currentDealer        :: Player  
 , currentBids          :: Maybe [PlayerBid]
 , currentTrick         :: Maybe Trick
 , currentTrump         :: Trump
 , completedTricks      :: [Trick] 
 , currentRound         :: Int
 , gameHistory          :: GameHistory}
  deriving (Show)