{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text                                 ( Text, pack )
import qualified Data.Text.Lazy as TL            ( fromStrict, unpack )
import qualified Data.ByteString.Lazy as BSL     ( toChunks )
import qualified Data.ByteString.Lazy.Char8 as C ( fromChunks, unpack )
import qualified Data.HashMap.Lazy as HML        ( lookup )
import Network.Wreq
import Control.Lens
import Prelude hiding (putStrLn)
import Data.Map as Map
import Data.List as List
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens (_String, key, nth, _Array)
import Control.Applicative
import Control.Monad
data HandID = HandID
  { handID :: String
  , label :: String
  , table :: String
  }

instance FromJSON HandID where
   parseJSON (Object v) =
    HandID <$> v .: "id"
           <*> v .: "label"
           <*> v .: "table"
   parseJSON _ = mzero

data IDList = IDList
  { status :: String
  , dataList  :: [HandID]
  }

data HandActions = HandActions
  { players :: [Player]
  , flop0 :: Maybe Card
  , flop1 :: Maybe Card
  , flop2 :: Maybe Card
  , turn :: Maybe Card
  , river :: Maybe Card
  }

data Player = Player
  { playerID :: String
  , name :: String
  , fid :: String
  , cards :: Maybe Card
  , image :: String
  , chipActions :: [ChipAction]
  , stack :: Integer
  , won :: Bool
  , onTheGame :: Bool
  }

parseChipString :: String -> Integer
parseChipString "" = 0
parseChipString s =
    let amt = init (init s) in
    case last s of
      'B' -> floor $ (read amt :: Double) * 1000000000
      'M' -> floor $ (read amt :: Double) * 1000000
      'K' -> floor $ (read amt :: Double) * 1000
      _   -> read s :: Integer



data ChipAction =
    Blind Integer
  | Raise Integer
  | Call Integer
  | Check
  | Fold
  deriving (Eq)

-- instance fromJSON ChipAction where
--   parseJSON (Object o) =
--     case HML.lookup "type" o of
--       Just (String "BLIND") -> Blind . parseChipString . show <$> o .: "chip"
--       _ -> mzero

{--
testGet = do
  r <- asJSON =<< get reqChipActions
  let p = r ^. responseBody . key "data" . _Array
  return p


testProcess r = r ^. responseStatus . statusCode
--}

startUrl = "https://www.turngs.com/games/poker/plugins/log/"
apiUrl = "app/index.php?accessToken="
accessToken = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xNzYuMTcwLjIyNyIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjA4NDU0NX0.9FUUv2gLet_Xi33YzYrHaXKzDu6uai1T7JPgrn_sshs"

extrasForChipActionsFor1Hand = "&action=detay&id=592882936a1c65876e49ef99"
extrasForListOf25HandIDs = "&action=list"


reqList = startUrl ++ apiUrl ++ accessToken ++  extrasForListOf25HandIDs
reqChipActions = startUrl ++ apiUrl ++ accessToken ++ extrasForChipActionsFor1Hand

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq)

instance Show Suit where
  show Diamonds = "d"
  show Hearts = "h"
  show Spades = "s"
  show Clubs = "c"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq,Ord,Enum,Bounded, Read)

instance Show Rank where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card =  Card Rank Suit deriving (Eq)
instance Show Card where
  show (Card r s) = show r ++ show s

indexToCard :: Int -> Maybe Card
indexToCard i
  | i >= 0 && i <= 12 = Just $ Card (toEnum i) Diamonds
  | i >= 13 && i <= 25 = Just $ Card (toEnum i) Hearts
  | i >= 26 && i <= 38 = Just $ Card (toEnum i) Spades
  | i >= 39 && i <= 51 = Just $ Card (toEnum i) Clubs
  | otherwise = Nothing


main :: IO ()
main = undefined