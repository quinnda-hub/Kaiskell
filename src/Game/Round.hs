module Game.Round where

import Models.State (GameState(..), Trick (..), RoundInfo (..), GameHistory (..))
import Models.Cards (shuffledDeck, dealCards, Card)
import System.Random (RandomGen(..))
import Models.Team (Team(..))
import Models.Players ( Player(hand) )
import Game.Core (rotateDealer, nextDealer)
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M
import Evaluator ( Evaluator )

startNewRound :: GameState -> Evaluator GameState
startNewRound game = do 
    let (hand1, hand2, hand3, hand4) = dealCards (shuffledDeck $ gen game)
        (_, gen') = split (gen game)
        p1        = dealCardsToPlayer (player1 $ team1 game) hand1
        p2        = dealCardsToPlayer (player2 $ team1 game) hand2
        p3        = dealCardsToPlayer (player1 $ team2 game) hand3
        p4        = dealCardsToPlayer (player2 $ team2 game) hand4
        t1        = (team1 game) { player1 = p1, player2 = p2}
        t2        = (team2 game) { player1 = p3, player2 = p4}
        dealer    = nextDealer (currentDealer game) game
        player    = (nextDealer $ nextDealer (currentDealer game) game) game
        cards     = maybe [] plays (currentTrick game)
        round     = currentRound game
        tricks    = completedTricks game
        
    
    bids  <- case currentBids game of 
        Nothing -> fail "Logic error: currentBids should never be Nothing at this point!"
        Just b  -> return b

    let GameHistory currentHistory = gameHistory game
        
    return game
        { gen = gen'
        , team1 = t1
        , team2 = t2
        , currentPlayer   = player
        , currentDealer   = dealer
        , currentBids     = Nothing
        , currentTrick    = Nothing
        , currentTrump    = Nothing
        , completedTricks = []
        , currentRound    = round + 1
        , gameHistory = GameHistory $ M.insert round (RoundInfo bids cards tricks) currentHistory
}

dealCardsToPlayer :: Player -> [Card] -> Player
dealCardsToPlayer player newCards = player { hand = newCards }
