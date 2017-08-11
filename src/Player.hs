{-# LANGUAGE OverloadedStrings #-}

module Player where

import           Card                (Card, cardUrlToCard)
import           ChipAction          (ChipAction (..), fixCalls,
                                      parseChipString)
import           Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON, parseJSON)
import           Data.Aeson.Types    (Object, Parser, Value (..), withObject,
                                      (.:))
import qualified Data.List           as L (filter)
import qualified Data.Map            as HM (Map, lookup)
import           Data.Maybe
_HERO_ID = "1728183"

data PlayerResp = PlayerResp
  { _playerID    :: !String
  , _name        :: !String
  , _fid         :: !String
  , _cards       :: ![String]
  , _image       :: !String
  , _chipActions :: ![[ChipAction]]
  , _stack       :: !Integer
  , _won         :: !Bool
--   , _onTheGame :: !Bool
  } deriving (Show)

data Player = Player
  { playerID    :: !String
  , name        :: !String
  , fid         :: !String
  , cards       :: ![Maybe Card]
  , image       :: !String
  , chipActions :: ![[ChipAction]]
  , stack       :: !Integer
  , won         :: !Bool
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

f :: HM.Map String [ChipAction] -> [[ChipAction]]
f m = case HM.lookup "0" m of
        Nothing -> [[]]
        Just l0 -> case HM.lookup "1" m of
            Nothing -> [l0]
            Just l1 -> case HM.lookup "2" m of
                Nothing -> [l0, l1]
                Just l2 -> case HM.lookup "3" m of
                    Nothing -> [l0, l1, l2]
                    Just l3 -> [l0, l1, l2, l3]

--getBlinds :: (p1, p2) -> (sb, bb)
getBlinds :: (Player, Player) -> (Player, Player)
getBlinds (p1,p2)
    | isSB p1 = (p1, p2)
    | isSB p2 = (p2, p1)
    | otherwise =
        let p1preflop = head (chipActions p1)
            p2preflop = head (chipActions p2)
            p1postflop = tail (chipActions p1)
            p2postflop = tail (chipActions p2)
        in
            case compare (length p1preflop) (length p2preflop) of
            GT -> (p1, p2)
            LT -> (p2, p1)
            _ -> case compare (fmap length p1postflop) (fmap length p2postflop) of
                 GT -> (p2, p1)
                 LT -> (p1, p2)
                 _ | isActionRaise (last p1preflop) -> (p1, p2)
                   | isActionRaise (last p2preflop) -> (p2, p1)
                   | any isActionRaise (lastActions p1postflop) -> (p2, p1)
                   | any isActionRaise (lastActions p2postflop) -> (p1, p2)
                   | otherwise -> (notHero [p1,p2], hero [p1,p2])
                    -- impossible to know who was sb so just pick other player as sb because I will raise 100% of posted sb's



playerNames :: (Player, Player) -> (String, String)
playerNames (p1, p2) = (name p1, name p2)

isActionCall (Call _) = True
isActionCall _        = False

isActionRaise (Raise _) = True
isActionRaise (Allin _) = True
isActionRaise _         = False

lastActions :: [[ChipAction]] -> [ChipAction]
lastActions = concatMap (take 1 . reverse)

lastActionsOnEveryStreetPostFlop :: Player -> [ChipAction]
lastActionsOnEveryStreetPostFlop ps =
    let cas = tail (chipActions ps) -- remove pre-flop since order is reversed
        lastActionList = fmap (take 1 . reverse) cas
    in concat lastActionList

cardsKnown :: [Maybe Card] -> Bool
cardsKnown = any isJust

blind :: Player -> Integer
blind p = case head (head (chipActions p)) of
    (Blind i) -> i
    _         -> -1

isSB :: Player -> Bool
isSB p = case head (head (chipActions p)) of
    (Blind 50) -> True
    (Blind 20) -> True
    _          -> False

isBB :: Player -> Bool
isBB p = case head (head (chipActions p)) of
    (Blind 100 ) -> True
    (Blind 40)   -> True
    _            -> False

screenName :: Player -> String
screenName p = name p ++ "_" ++ playerID p

seat :: [Player] -> Player -> Int
seat [x,_] p = if p == x then 1 else 2

hero :: [Player] -> Player
hero ps = head (L.filter (\p -> playerID p == _HERO_ID) ps)

notHero :: [Player] -> Player
notHero ps = head (L.filter (\p -> playerID p /= _HERO_ID) ps)

winners :: [Player] -> [Player]
winners = L.filter won

sortWinnerLast :: [Player] -> [Player]
sortWinnerLast [p1, p2] = if won p1 then [p2, p1] else [p1, p2]
sortWinnerLast ps       = ps

tie :: [Player] -> Bool
tie ps = length (L.filter won ps) == 2

playersChipActionsOfStreet :: Player -> Int -> Maybe [(String, ChipAction)]
playersChipActionsOfStreet p street
    | street >= length (chipActions p) = Nothing
    | otherwise =
        let sn = screenName p
            f = (,) sn
        in Just (fmap f (chipActions p !! street ))
