{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where


import GHC.Generics
import GHC.Exts as E (fromList)
import Data.Char (isDigit)
import Data.Text     as T                            (pack, unpack)
import qualified Data.Text.Lazy as TL            ( fromStrict, unpack )
import qualified Data.ByteString.Lazy as BSL     ( toChunks , pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as C ( fromChunks, pack)
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.HashMap.Strict as HM
-- import qualified Data.Text.Lazy.IO as T
-- import qualified Data.Text.Lazy.Encoding as T
-- import qualified Data.Text.Lazy as T (unpack)
import Network.Wreq
import Control.Lens
import Prelude hiding (putStrLn)
import Data.Map as Map
import Data.List as List
import Data.Aeson
import Data.Aeson.Types as Ty
import Data.Aeson.Lens (_String, key, nth, _Array)
import Control.Applicative
import Control.Monad
import qualified Data.Vector as V


val :: Value
val = Object $ E.fromList [
  ("numbers", Array $ E.fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True) ]

-- parseTuple :: Value -> Parser (String, Bool)
parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)


parseHandIDs :: Value -> Parser [HandID]
parseHandIDs = withObject "HandIDs" $ \o -> do
  o .: "data"

parseCard :: Value -> Parser (Maybe Card)
parseCard = withText "cardURL" $ \x ->
    return $ cardUrlToCard (T.unpack x)


test1 = parseMaybe parseCard =<< decode exampleCardUrl

-- parseCards :: Value -> Parser ![String]
-- parseCards = withObject "cards" $ \o ->
--     c <- o .: "cards"
--     let cs = parseCard c
--     return c
-- Object [(String, Value)]
-- [Number Scientific]


parseArray :: Value -> Parser [(String, Bool)]
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _           = fail "expected an array"













-- _TLD = "https://www.turngs.com/games/poker/plugins/log/app/index.php?"
-- _ACCESS_KEY = "accessToken="
-- _ACCESS_VALUE = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xNzYuMTcwLjIyNyIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjA4NDU0NX0.9FUUv2gLet_Xi33YzYrHaXKzDu6uai1T7JPgrn_sshs"
-- _EXTRAS_ID_LIST = "&action=list"
-- _EXTRAS_CHIP_ACTIONS = "&action=detay&id=592882936a1c65876e49ef99"

_URL_ID_LIST = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjE3NjE3N30.2XnBNtXhjZwnTv3ifo8cQJjkLyVfh_s06xIXCQDUjgs&action=list"
_URL_CHIP_ACTIONS = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjE3NjE3N30.2XnBNtXhjZwnTv3ifo8cQJjkLyVfh_s06xIXCQDUjgs&action=detay&id=592b343035a9080c68e768f6"


data HandID = HandID
  { handID :: !String
  , label :: !String
  , table :: !String
  } deriving (Show)

instance FromJSON HandID where
   parseJSON (Object v) =
    HandID <$> v .: "id"
           <*> v .: "label"
           <*> v .: "table"
   parseJSON _ = mzero


data HandActions = HandActions
  { players :: ![Player]
  , flop_0 :: !String
  , flop_1 :: !String
  , flop_2 :: !String
  , turn :: !String
  , river :: !String
  } deriving (Show)

instance FromJSON HandActions where
    parseJSON (Object v) =
        HandActions <$> v .: "players"
                    <*> v .: "flop_0"
                    <*> v .: "flop_1"
                    <*> v .: "flop_2"
                    <*> v .: "turn"
                    <*> v .: "river"
    parseJSON _ = mzero

data Player = Player
  { playerID :: !String
  , name :: !String
  , fid :: !String
  , cards :: ![String]
  , image :: !String
  , chipActions :: ![[ChipAction]]
  , stack :: !String
  , won :: !Bool
  , onTheGame :: !Bool
  } deriving (Show)



instance FromJSON Player where
    parseJSON (Object v) =
        Player <$> v .: "id"
               <*> v .: "name"
               <*> v .: "fid"
               <*> v .: "cards"
               <*> v .: "image"
               <*> (v .: "actions" :: Parser [[ChipAction]])
               <*> v .: "chip"
               <*> v .: "won"
               <*> v .: "onTheGame"
    parseJSON _ = mzero

data ChipAction =
    Blind Integer
  | Raise Integer
  | Call Integer
  | Check
  | Fold
  deriving (Eq, Show)


-- parsePlayer :: Value -> Parser Player

parseChipActions :: Maybe Value -> Parser [ChipAction]
parseChipActions (Just (Array arr)) = mapM parseJSON (V.toList arr)
parseChipActions _           = fail "expected an array of ChipActions"


-- parseChipActions :: Maybe Value -> Parser [ChipAction]
-- parseChipActions (Just val) = withObject "ChipActions" ( \o -> do
--   o .: "actions") val
-- parseChipActions Nothing = mzero




parseChipString :: String -> Integer
parseChipString "" = 0
parseChipString s =
    let amt = init (init s) in
    case last s of
      'B' -> floor $ (read amt :: Double) * 1000000000
      'M' -> floor $ (read amt :: Double) * 1000000
      'K' -> floor $ (read amt :: Double) * 1000
      _   -> read s :: Integer

showInt :: Int -> String
showInt = show


instance FromJSON ChipAction where
  parseJSON (Object o) =
    case HML.lookup "type" o of
      Just (String "BLIND") -> ((Blind . parseChipString . showInt) <$> o .: "chip" <|> (Blind . parseChipString <$> o .: "chip"))
      Just (String "RAISE") -> ((Raise . parseChipString . showInt) <$> o .: "chip" <|> (Blind . parseChipString <$> o .: "chip"))
      Just (String "CALL") -> ((Call . parseChipString . showInt) <$> o .: "chip" <|> (Blind . parseChipString <$> o .: "chip"))
      Just (String "CHECK") -> return Check
      Just (String "FOLD") -> return Main.Fold
  parseJSON _ = mzero
-- <*> (v .: "Value" <|> (showInt <$> v .: "Value"))
{--
testGet = do
  r <- asJSON =<< get reqChipActions
  let p = r ^. responseBody . key "data" . _Array
  return p


testProcess r = r ^. responseStatus . statusCode
--}



{--
** CARD TYPE
-}

data Card =  Card Rank Suit deriving (Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight |
            Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq,Ord,Enum,Bounded, Read)

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Eq)

instance Show Card where
  show (Card r s) = show r ++ show s

instance Show Suit where
  show Diamonds = "d"
  show Hearts = "h"
  show Spades = "s"
  show Clubs = "c"

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



cardUrlToCard :: String -> Maybe Card
cardUrlToCard s =
    let n = (take 2 . reverse . take 6 . reverse) s
    in
        if isDigit (head n)
            then indexToCard (read n :: Int)
            else if isDigit (last n)
                then indexToCard (read (tail n) :: Int)
                else Nothing


indexToCard :: Int -> Maybe Card
indexToCard i
  | i >= 0 && i <= 12 = Just $ Card (toEnum i) Diamonds
  | i >= 13 && i <= 25 = Just $ Card (toEnum (i-13)) Hearts
  | i >= 26 && i <= 38 = Just $ Card (toEnum (i-26)) Spades
  | i >= 39 && i <= 51 = Just $ Card (toEnum (i-39)) Clubs
  | otherwise = Nothing

-- TODO check if this is what we get in response (we removed \'s)
exampleCardUrl = C.pack "http://adv3.fbtrnpkr.info/kartlar/0.png"

testHandIDData = C.pack "[{\"id\":\"592b343035a9080c68e768f6\",\"label\":\"28 (23:33)\",\"table\":\"Learning - Advanced 3 \"},{\"id\":\"592b33ee56a9083326fc42fa\",\"label\":\"28 (23:32)\",\"table\":\"Learning - Advanced 3 \"}]"

testPlayer = C.pack "{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}"
testHandActions = C.pack "{\"players\":[{\"id\":\"1728183\",\"name\":\"Eitan\",\"fid\":\"10153181502456109\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/24.png\",\"http://adv3.fbtrnpkr.info/kartlar/34.png\"],\"image\":\"https://graph.facebook.com/10153181502456109/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":100},{\"type\":\"RAISE\",\"chip\":\"3.23 K\"}]],\"chip\":\"19.76 K\",\"won\":true,\"onTheGame\":true},{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}],\"flop_0\":\"http://adv3.fbtrnpkr.info/kartlar/6.png\",\"flop_1\":\"http://adv3.fbtrnpkr.info/kartlar/45.png\",\"flop_2\":\"http://adv3.fbtrnpkr.info/kartlar/48.png\",\"turn\":\"http://adv3.fbtrnpkr.info/kartlar/0.png\",\"river\":\"http://adv3.fbtrnpkr.info/kartlar/37.png\"}"
chipAction = C.pack "{\"type\": \"BLIND\",\"chip\": \"50 K\"}"
-- type Resp = Response (Map String Value)
--getLatest25HandIDs :: [String]
getLatest25HandIDs = do
--    r <- asJSON =<< get _URL_ID_LIST :: IO Resp
--   let p =  r ^. responseBody . key "data" . _Array
   r <- get _URL_ID_LIST
--    let p =  r ^. responseBody . key "data" . _Array
   return (parseMaybe parseHandIDs =<< decode (r ^. responseBody))
--    let p = Map.lookup "data" $ r ^. responseBody
--    let s = parseMaybe parse =<< decode bs
--    q <- mapM parseJSON p :: Maybe (Parser [HandID])
--    return q
--    let s = case p of
--                (Just (Ty.Array (c))) -> return (Ty.Array c)
--                _ -> return Null

--    let s = parseJSON p ::
--    return (p)


main :: IO ()
main = undefined