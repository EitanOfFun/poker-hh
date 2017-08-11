{-# LANGUAGE OverloadedStrings #-}

module HandActions where

import Player (Player, PlayerResp, fixPlayer)
import Card (Card, cardUrlToCard)
import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Types (Object, Value (..), (.:))
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))

data HandActionsResp = HandActionsResp
  { _players :: ![PlayerResp]
  , _flop_0 :: !String
  , _flop_1 :: !String
  , _flop_2 :: !String
  , _turn :: !String
  , _river :: !String
  } deriving (Show)

data HandActions = HandActions
  { players :: ![Player]
  , flop_0 :: !(Maybe Card)
  , flop_1 :: !(Maybe Card)
  , flop_2 :: !(Maybe Card)
  , turn :: !(Maybe Card)
  , river :: !(Maybe Card)
  } deriving (Show, Eq)

fixHandActions :: HandActionsResp -> HandActions
fixHandActions (HandActionsResp h1 h2 h3 h4 h5 h6) = HandActions
    { players = fmap fixPlayer h1
    , flop_0 = cardUrlToCard h2
    , flop_1 = cardUrlToCard h3
    , flop_2 = cardUrlToCard h4
    , turn = cardUrlToCard h5
    , river = cardUrlToCard h6
    }

instance FromJSON HandActionsResp where
    parseJSON (Object v) =
        HandActionsResp <$> v .: "players"
                        <*> v .: "flop_0"
                        <*> v .: "flop_1"
                        <*> v .: "flop_2"
                        <*> v .: "turn"
                        <*> v .: "river"
    parseJSON _ = mzero
