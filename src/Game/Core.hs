module Game.Core where

import Data.Map ( Map )
import qualified Data.Map as M
import Models.Cards ( dealCards, shuffledDeck )
import Models.Players ( Player(Player) )
import System.Random ( StdGen, split )
import Models.State
import Models.Team ( Team(Team, player1, player2) )
import qualified Control.Applicative as Map
import Data.Maybe (maybeToList)

initializeGame :: String ->
                  String ->
                  String ->
                  String ->
                  StdGen ->
                  GameState
initializeGame name1 name2 name3 name4 gen =
    let (hand1, hand2, hand3, hand4) = dealCards (shuffledDeck gen)
        player1   = Player name1 hand1
        player2   = Player name2 hand2
        player3   = Player name3 hand3
        player4   = Player name4 hand4
        teamA     = Team player1 player3 0
        teamB     = Team player2 player4 0
        history   = GameHistory M.empty
        (_, gen') = split gen
    in GameState gen teamA teamB [] player1 player1 Nothing Nothing Nothing [] 0 history

gameLoop :: GameState -> IO GameState
gameLoop = undefined

nextDealer :: Player -> GameState -> Player
nextDealer currentDealer gameState
    | currentDealer == player1 (team1 gameState) = player1 (team2 gameState)
    | currentDealer == player1 (team2 gameState) = player2 (team2 gameState)
    | currentDealer == player2 (team2 gameState) = player2 (team1 gameState)
    | otherwise                                  = player1 (team1 gameState)

rotateDealer :: GameState -> GameState
rotateDealer gameState =
    let newDealer = nextDealer (currentDealer gameState) gameState
    in gameState { currentDealer = newDealer}
