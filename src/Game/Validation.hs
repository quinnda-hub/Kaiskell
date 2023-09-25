module Game.Validation where

import Data.List (nub)
import Data.Maybe ( isNothing
                  , fromJust)
import Models.Cards ( Card (suit) )
import Models.Bids ( PlayerBid(bid) )
import Models.Constants ( BidValue(BidPass) )
import Control.Monad ( when )
import Evaluator ( Evaluator )
import Models.Players ( Player
                      , hand)
import Models.State ( GameState(currentPlayer
                               , currentBids
                               , currentDealer
                               , currentTrick)
                    , Trick(plays)
                    , CardPlay (playedCard))

validateHigherBid :: [PlayerBid] -> Player -> Player -> BidValue -> Evaluator ()
validateHigherBid currentBids dealer currentPlayer newBid
    | newBid == BidPass = pure ()
    | dealer == currentPlayer && newBid >= maxBid = pure ()
    | newBid > maxBid = pure ()
    | otherwise = fail "Your bid must be higher than the current bid."
    where maxBid = maximum (map bid currentBids)

validateDealerBid :: Player -> Player -> BidValue -> [PlayerBid] -> Evaluator ()
validateDealerBid dealer currentPlayer newBid allBids =
    when (dealer == currentPlayer &&
        all (== BidPass) bs &&
         newBid == BidPass) $ fail "As the dealer, you must bid when everyone else passes."
    where bs = map bid allBids

validateBid :: GameState -> Player -> PlayerBid -> Evaluator ()
validateBid game player newBid =
    case currentBids game of
        Nothing   -> pure ()  -- There's no previous bid, so no need to validate against them.
        Just bids -> do
            validateHigherBid bids
                              (currentDealer game)
                              (currentPlayer game)
                              (bid newBid)
            validateDealerBid (currentDealer game)
                              (currentPlayer game)
                              (bid newBid)
                              bids

validatePlayerTurn :: GameState -> Player -> Evaluator ()
validatePlayerTurn game player 
    | currentPlayer game == player = pure ()
    | otherwise                    = fail "It's not your turn to play."

validateCardInHand :: Player -> Card -> Evaluator ()
validateCardInHand player card 
    | card `elem` hand player = pure ()
    | otherwise = fail "You can't play a card you don't have in your hand."

validateSuitMatch :: GameState -> Player -> Card -> Evaluator ()
validateSuitMatch game player card 
    | isNothing $ currentTrick game         = pure ()
    | leadSuit == cardSuit                  = pure ()
    | leadSuit `notElem` suitsInHand player = pure ()
    | otherwise = fail $ "You must play a card that matches the suit of the" ++
                         "leading card if you have one in your hand."
    where 
        leadCard = playedCard . last . plays . fromJust $ currentTrick game
        leadSuit = suit leadCard
        cardSuit = suit card 
        suitsInHand player = nub $ map suit (hand player)

validateCardPlay :: GameState -> Player -> Card -> Evaluator ()
validateCardPlay game player card = do
    validatePlayerTurn game player 
    validateCardInHand player card
    validateSuitMatch game player card