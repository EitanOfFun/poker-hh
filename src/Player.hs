{-# LANGUAGE OverloadedStrings #-}

module Player where

import Card
import ChipAction
import Data.Aeson
import Data.Aeson.Types as Ty
import Control.Applicative
import Control.Monad

data PlayerResp = PlayerResp
  { _playerID :: !String
  , _name :: !String
  , _fid :: !String
  , _cards :: ![String]
  , _image :: !String
  , _chipActions :: ![[ChipAction]]
  , _stack :: !Integer
  , _won :: !Bool
  , _onTheGame :: !Bool
  } deriving (Show)

data Player = Player
  { playerID :: !String
  , name :: !String
  , fid :: !String
  , cards :: ![Maybe Card]
  , image :: !String
  , chipActions :: ![[ChipAction]]
  , stack :: !Integer
  , won :: !Bool
  , onTheGame :: !Bool
  } deriving (Show)

fixPlayer :: PlayerResp -> Player
fixPlayer (PlayerResp p1 p2 p3 p4 p5 p6 p7 p8 p9) = Player
   { playerID = p1
   , name = p2
   , fid = p3
   , cards = fmap cardUrlToCard p4
   , image = p5
   , chipActions = p6
   , stack = p7
   , won = p8
   , onTheGame = p9
   }

instance FromJSON PlayerResp where
    parseJSON (Object v) =
        PlayerResp <$> v .: "id"
               <*> v .: "name"
               <*> v .: "fid"
               <*> v .: "cards"
               <*> v .: "image"
               <*> (v .: "actions" :: Parser [[ChipAction]])
               <*> (v .: "chip" <|> (parseChipString <$> v .: "chip"))
               <*> v .: "won"
               <*> v .: "onTheGame"
    parseJSON _ = mzero


