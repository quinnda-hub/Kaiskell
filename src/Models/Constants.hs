module Models.Constants (BidValue(..)) where

data BidValue = BidPass    | BidSeven    | BidSevenNo  | BidEight   | 
                BidEightNo | BidNine     | BidNineNo   | BidTen     | 
                BidTenNo   | BidEleven   | BidElevenNo | BidTwelve  | BidTwelveNo
                deriving (Show, Enum, Eq, Bounded, Ord)
                