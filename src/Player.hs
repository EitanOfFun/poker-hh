{-# LANGUAGE OverloadedStrings #-}

module Player where

import Card
import ChipAction
import Data.Aeson
import Data.Aeson.Types as Ty
import Control.Applicative
import Control.Monad
import Control.Lens
import Data.List as L
import Data.Map as Map
_HERO_ID = "1728183"

data PlayerResp = PlayerResp
  { _playerID :: !String
  , _name :: !String
  , _fid :: !String
  , _cards :: ![String]
  , _image :: !String
  , _chipActions :: ![[ChipAction]]
  , _stack :: !Integer
  , _won :: !Bool
--   , _onTheGame :: !Bool
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
--   , onTheGame :: !Bool
  } deriving (Show, Eq)

fixPlayer :: PlayerResp -> Player
fixPlayer (PlayerResp p1 p2 p3 p4 p5 p6 p7 p8) = Player
   { playerID = p1
   , name = p2
   , fid = p3
   , cards = fmap cardUrlToCard p4
   , image = p5
   , chipActions = fmap fixCalls p6
   , stack = p7
   , won = p8
--    , onTheGame = p9
   }

instance FromJSON PlayerResp where
    parseJSON (Object v) =
        PlayerResp <$> v .: "id"
               <*> v .: "name"
               <*> v .: "fid"
               <*> v .: "cards"
               <*> v .: "image"
--                <*> (v .: "actions" :: Parser [[ChipAction]])
               <*> ((v .: "actions" :: Parser [[ChipAction]]) <|> (f <$> v .: "actions"))
               <*> (v .: "chip" <|> (parseChipString <$> v .: "chip"))
               <*> v .: "won"
--                <*> v .: "onTheGame"
    parseJSON _ = mzero

f :: Map String [ChipAction] -> [[ChipAction]]
f m = case Map.lookup "0" m of
        Nothing -> [[]]
        Just l0 -> case Map.lookup "1" m of
            Nothing -> [l0]
            Just l1 -> case Map.lookup "2" m of
                Nothing -> [l0, l1]
                Just l2 -> case Map.lookup "3" m of
                    Nothing -> [l0, l1, l2]
                    Just l3 -> [l0, l1, l2, l3]

blind :: Player -> Integer
blind p = case head (head (chipActions p)) of
    (Blind i) -> i
    _ -> (-1)

isSB :: Player -> Bool
isSB p = case head (head (chipActions p)) of
    (Blind 50000000) -> True
    (Blind 20000000) -> True
    _ -> False

isBB :: Player -> Bool
isBB p = case head (head (chipActions p)) of
    (Blind 100000000 ) -> True
    (Blind 40000000) -> True
    _ -> False

screenName :: Player -> String
screenName p = (name p) ++ "_" ++ (playerID p)

seatOfSB :: [Player] -> Int
seatOfSB ps = _seatOfSB ps 1

_seatOfSB :: [Player] -> Int -> Int
_seatOfSB [] _ = 0
_seatOfSB (p:ps) i = if isSB p then i else _seatOfSB ps (i + 1)

hero :: [Player] -> Player
hero ps = head (L.filter (\p -> playerID p == _HERO_ID) ps)

winner :: [Player] -> Player
winner ps = head (L.filter (\p -> won p) ps)



playersChipActionsOfStreet :: Player -> Int -> Maybe [(String, ChipAction)]
playersChipActionsOfStreet p street
    | street >= length (chipActions p) = Nothing
    | otherwise =
        let sn = screenName p
            f = (,) sn
        in Just (fmap f ((chipActions p) !! street ))