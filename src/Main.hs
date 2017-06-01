{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import Card
import ChipAction
import HandID
import Player
import HandActions
import GHC.Generics
import GHC.Exts as E (fromList)
import Data.Char (isDigit)
import Data.Text     as T                            (pack, unpack)
import qualified Data.Text.Lazy as TL            ( fromStrict, unpack )
import qualified Data.ByteString.Lazy as BSL     ( toChunks , pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as C ( fromChunks, pack, writeFile, readFile)
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.HashMap.Strict as HM
-- import qualified Data.Text.Lazy.IO as T
-- import qualified Data.Text.Lazy.Encoding as T
-- import qualified Data.Text.Lazy as T (unpack)
import Network.Wreq
import Control.Lens
import Prelude hiding (putStrLn)
import Data.Map as Map
import qualified Data.Vector as V
import Data.List as List
import Data.Aeson
import Data.Aeson.Types as Ty
import Data.Aeson.Lens (_String, key, nth, _Array)
import Control.Applicative
import Control.Monad



-- TODO update automaticaly to new url
_NEW_ACCESS_TOKEN_AFTER = "var apiUrl = \"app/index.php?accessToken=\" + \""
_BASE_URL = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken="
_ACCESS_TOKEN = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjMyNjU4N30.jsvkjSbMQiOyS2qBYbf9Xu1WaQUogWc10fk_rD5fhyw"
_URL_ID_LIST = _BASE_URL ++ _ACCESS_TOKEN ++ "&action=list"
_SAMPLE_ID = "592d5d67661c65f81004cff5"
_sdf =   "https://www.turngs.com/games/poker/plugins/log/index.php?accessToken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjI2MzQ4NX0.UDf-qk89osAFRmslo3iYEKusyYUQvNkqtE8vV80m-GI#"
_URL_HAND_ACTIONS = _BASE_URL ++ _ACCESS_TOKEN ++ "&action=detay&id="
_FILE_PATH = "database/hh-json/"



testHandIDData = C.pack "[{\"id\":\"592b343035a9080c68e768f6\",\"label\":\"28 (23:33)\",\"table\":\"Learning - Advanced 3 \"},{\"id\":\"592b33ee56a9083326fc42fa\",\"label\":\"28 (23:32)\",\"table\":\"Learning - Advanced 3 \"}]"

testPlayer = C.pack "{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}"
testHandActions = C.pack "{\"players\":[{\"id\":\"1728183\",\"name\":\"Eitan\",\"fid\":\"10153181502456109\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/24.png\",\"http://adv3.fbtrnpkr.info/kartlar/34.png\"],\"image\":\"https://graph.facebook.com/10153181502456109/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":100},{\"type\":\"RAISE\",\"chip\":\"3.23 K\"}]],\"chip\":\"19.76 K\",\"won\":true,\"onTheGame\":true},{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}],\"flop_0\":\"http://adv3.fbtrnpkr.info/kartlar/6.png\",\"flop_1\":\"http://adv3.fbtrnpkr.info/kartlar/45.png\",\"flop_2\":\"http://adv3.fbtrnpkr.info/kartlar/48.png\",\"turn\":\"http://adv3.fbtrnpkr.info/kartlar/0.png\",\"river\":\"http://adv3.fbtrnpkr.info/kartlar/37.png\"}"
chipAction = C.pack "{\"type\": \"BLIND\",\"chip\": \"50 K\"}"


reqLatest25HandIDs :: IO (Maybe [HandID])
reqLatest25HandIDs = do
   r <- get _URL_ID_LIST
   return (parseMaybe parseHandIDs =<< decode (r ^. responseBody))

reqHandActions :: String -> IO (Either String HandActions)
reqHandActions hID = do
    r <- get (_URL_HAND_ACTIONS ++ hID)
    let decoded = (eitherDecode (r ^. responseBody) :: Either String HandActionsResp)
        fixed = fmap fixHandActions decoded
    return (fixed)

-- decodeHandActions :: String -> (Either String HandActions)
-- decodeHandActions s = do
--     let decoded = eitherDecode s :: Either String HandActionsResp
--         fixed = fmap fixHandActions decoded
--     return (fixed)

writeHandActionsUnparsedJSON :: HandID -> IO ()
writeHandActionsUnparsedJSON hID =
    let file = _FILE_PATH ++ (HandID.handID hID) ++(HandID.label (fixLabel hID)) ++  ".txt"
    in do
        n <- doesFileExist (file)
        case n of
            True -> return ()
            False -> do
                r <- get (_URL_HAND_ACTIONS ++ (HandID.handID hID))
                C.writeFile (file) (r ^. responseBody)



writeLatest25Hands = do
    r <-  reqLatest25HandIDs
    case r of
        (Just hIDs) -> (mapM_ writeHandActionsUnparsedJSON hIDs)

-- readHand :: IO (Either String HandActions)
-- readHand :: IO (Either String [String])
readHand = do
    f <- fmap (head . (drop 7)) (listDirectory _FILE_PATH)
    r <- fmap C.pack (readFile (_FILE_PATH ++ f))
--     return r
    let decoded = (eitherDecode (r ) :: Either String HandActionsResp)
        fixed = fmap fixHandActions decoded
        hh = fmap handToHH fixed
    return (showEither hh)



-- handToHH :: HandActions -> String
handToHH (HandActions ps c1 c2 c3 c4 c5) = do
    if length ps >= 3
        then [(show (length ps)) ++ " players - too many players"]
    else let sb = head $ List.filter isSB ps
             bb = head $ List.filter isBB ps
             seat1 = head ps
             seat2 = last ps
             preflopSBActions = playersChipActionsOfStreet sb 0
             preflopBBActions = playersChipActionsOfStreet bb 0
             preFlopAction = fmap (drop 2) (mergeMaybeList preflopSBActions preflopBBActions)
             s = [ "Seat 1: " ++ (screenName seat1) ++ " ($" ++ (show (stack seat1)) ++ ")"
                 , "Seat 2: " ++ (screenName seat2) ++ " ($" ++ (show (stack seat2)) ++ ")"
                 , (screenName sb) ++ " posts the small blind of $" ++ (show (blind sb))
                 , (screenName bb) ++ " posts the big blind of $" ++ (show (blind bb))
                 , "The button is in seat #" ++ (show (seatOfSB ps))
                 , "*** HOLE CARDS ***"
                 , "Dealt to " ++ (screenName (hero ps)) ++ " " ++ (hhCards (cards (hero ps)))
--                  , "PREFLOP:  " ++ (showMaybe (fmap showStreetActions preFlopAction))
                 , (showStreetActions bb sb [c1,c2,c3,c4,c5] 0)
                 , (showStreetActions bb sb [c1,c2,c3,c4,c5] 1)
                 , (showStreetActions bb sb [c1,c2,c3,c4,c5] 2)
                 , (showStreetActions bb sb [c1,c2,c3,c4,c5] 3)
                 , "*** SUMMARY ***"
                 , "Total pot $" ++ " | Rake $0"
                 ]
         in s

showStreetActions bb sb (c1:c2:c3:c4:c5:[]) street =
    let sbActions = playersChipActionsOfStreet sb street
        bbActions = playersChipActionsOfStreet bb street
    in
        if street == 0
            then showMaybe $ fmap showStreetChipActions (fmap (drop 2) (mergeMaybeList sbActions bbActions))
            else
            let merged = mergeMaybeList bbActions sbActions
            in case merged of
                    Just m -> case street of
                                        1 -> "*** FLOP *** " ++ (hhCards (c1:c2:c3:[])) ++ "\n" ++ (showStreetChipActions m)
                                        2 -> "*** TURN *** " ++ (hhCards (c1:c2:c3:c4:[])) ++ "\n" ++ (showStreetChipActions m)
                                        3 -> "*** RIVER *** " ++ (hhCards (c1:c2:c3:c4:c5:[])) ++ "\n" ++ (showStreetChipActions m)
                                        _ -> ""
                    _ -> ""



showMaybe :: Show a => Maybe a -> String
showMaybe Nothing = ""
showMaybe (Just a) = show a

showEither :: (Show a, Show b) => Either b a -> String
showEither (Left s) = show s
showEither (Right a) = show a

mergeMaybeList :: Maybe [a] -> Maybe [a] -> Maybe [a]
mergeMaybeList Nothing _ = Nothing
mergeMaybeList _ Nothing = Nothing
mergeMaybeList (Just l1) (Just l2) = Just $ merge l1 l2

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

--     return(decodeHandActions r)
--     return decoded


--     case r of
--         (Just handIDs) -> do
--             t <- fmap writeHandActionsUnparsedJSON (fmap HandID.handID handIDs)
--             return t
--     return r

--     l <- reqLatest25HandIDs
--     case (fmap . fmap) HandID.handID l of
--         (Just c) -> do (sequence $  fmap writeHandActionsUnparsedJSON c)
--         Nothing -> return [()]



-- writeHandActionsUnparsedJSON ::
main :: IO ()
main = undefined
-- main = do
--     h <- readHand
--     case h of
--         (Right hand) -> do
--             putStr hand
--         (Left e) -> do putStr "dfg"