module Models.Cards where

import Utils.Shuffle (shuffle)
import System.Random (StdGen)

data Suit = Hearts | Diamonds | Clubs | Spades
            deriving (Show, Enum, Bounded, Eq)

data Rank = Two   | Three | Four | Five | Six  |
            Seven | Eight | Nine | Ten  | Jack |
            King  | Queen | Ace
            deriving (Show, Enum, Bounded, Eq)

data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving Eq

instance Show Card where
    show (Card r s) = show r ++ " of " ++ show s

type Deck = [Card] 

createDeck :: [Card]
createDeck = [Card r s | r <- [Seven .. maxBound], s <- [minBound .. maxBound],
             not (r == Seven && (s == Spades || s == Hearts))] ++ 
             [Card Five Hearts, Card Three Spades]

shuffledDeck :: StdGen -> [Card]
shuffledDeck gen = fst $ shuffle createDeck gen

dealCards :: [Card] -> ([Card], [Card], [Card], [Card])
dealCards deck = (take 8 deck, 
                 take 8 $ drop 8 deck, 
                 take 8 $ drop 16 deck,
                 take 8 $ drop 24 deck)
