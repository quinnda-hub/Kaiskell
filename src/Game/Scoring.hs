module Game.Scoring where

import Evaluator ( Evaluator )
import Models.State 
  ( Trick(plays)
  , CardPlay(playedCard, cardPlayer)
  , GameState 
    ( currentBids
    , currentTrick
    , completedTricks
    , team1
    , team2
    )
  )
import Models.Cards 
  ( Card(Card)
  , Rank(Five, Three)
  , Suit(Hearts, Spades) 
  )
import Models.Constants (BidValue(..))
import Data.Foldable 
  ( Foldable(foldl')
  , maximumBy
  )
import Models.Bids (PlayerBid(..))
import Models.Team (Team(..))
import Models.Players (Player)
import Data.Ord (comparing)
import Data.Maybe (maybeToList)

bid2Int :: BidValue -> Int
bid2Int bid = case bid of
  BidPass     -> 0
  BidSeven    -> 7
  BidSevenNo  -> 7
  BidEight    -> 8
  BidEightNo  -> 8
  BidNine     -> 9
  BidNineNo   -> 9
  BidTen      -> 10
  BidTenNo    -> 10
  BidEleven   -> 11
  BidElevenNo -> 11
  BidTwelve   -> 12
  BidTwelveNo -> 12

isNoTrump :: BidValue -> Bool
isNoTrump bid = case bid of
  BidSevenNo  -> True
  BidEightNo  -> True
  BidNineNo   -> True
  BidTenNo    -> True
  BidElevenNo -> True
  BidTwelveNo -> True
  _           -> False

scoreTrick :: Trick -> Int
scoreTrick trick
  | threeOfSpades && fiveOfHearts = 3
  | threeOfSpades                 = -2
  | fiveOfHearts                  = 6
  | otherwise                     = 1
  where
    threeOfSpades = Card Three Spades `elem` map playedCard (plays trick)
    fiveOfHearts  = Card Five Hearts `elem` map playedCard (plays trick)

scoreBiddingTeam :: BidValue -> [Trick] -> Int
scoreBiddingTeam bid tricks =
  let totalScore = foldl' (\acc trick -> acc + scoreTrick trick) 0 tricks
  in if isNoTrump bid && totalScore >= bid2Int bid
    then 2 * totalScore
    else totalScore

scoreNonBiddingTeam :: BidValue ->
                       [Trick]  ->
                       Int      -> -- Score of the non bidding team.
                       Int
scoreNonBiddingTeam bid tricks score 
  | score < 47     = score + totalScore 
  | totalScore < 0 = score + totalScore 
  | otherwise      = score 
  where 
    totalScore = foldl' (\acc trick -> acc + scoreTrick trick) 0 tricks

scoreRound :: GameState -> Evaluator GameState
scoreRound game = do 
  -- Check if the current round is over.
  let roundOver = isRoundOver game

  if not roundOver
    then return game
    else do
      bids <- case currentBids game of 
                Nothing -> fail "No bid information found."
                Just b  -> return b

      let tricks        = completedTricks game
          highestBidder = bidPlayer $ maximumBy (comparing bid) bids
          highestBid    = maximum $ map bid bids
          biddingTeam   = teamFromPlayer game highestBidder
          otherTeam     = if biddingTeam == team1 game 
                          then team2 game 
                          else team1 game
          bidderScore   = scoreBiddingTeam highestBid tricks
          otherScore    = scoreNonBiddingTeam highestBid tricks (teamScore otherTeam)
          game'         = updateTeamScores game bidderScore otherScore 

      return game'

teamFromPlayer :: GameState -> Player -> Team
teamFromPlayer game player 
  | player1 team == player || player2 team == player = 
    team1 game 
  | otherwise = team2 game 
  where team = team1 game 

updateTeamScores :: GameState 
                 -> Int -- Bidding team score.
                 -> Int -- Other team score.
                 -> GameState
updateTeamScores game team1ScoreChange team2ScoreChange = 
    game { team1 = updatedTeam1, team2 = updatedTeam2 }
  where
    t1 = team1 game
    t2 = team2 game

    updatedTeam1 = t1 { teamScore = teamScore t1 + team1ScoreChange }
    updatedTeam2 = t2 { teamScore = teamScore t2 + team2ScoreChange }

isRoundOver :: GameState -> Bool
isRoundOver game = all (\p -> playerCardCount p == 8) allPlayers
  where
    playedCards = concatMap plays $
                  completedTricks game ++ maybeToList (currentTrick game)
    allPlayers  = [ player1 (team1 game), player2 (team1 game)
                  , player1 (team2 game), player2 (team2 game)]
    playerCardCount player = length (filter (\pc -> cardPlayer pc == player) playedCards)                  
