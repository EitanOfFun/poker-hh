{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Directory
import Card
import ChipAction
import HandID
import Player
import HandActions
import TurnDate
import Data.Maybe
import GHC.Generics
import GHC.Exts as E (fromList)
import Data.Char (isDigit)
import Data.Text     as T                            (pack, unpack)
import qualified Data.Text.Lazy as TL            ( fromStrict, unpack )
import qualified Data.ByteString.Lazy as BSL     ( toChunks , pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as C ( fromChunks, pack, writeFile, readFile)
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.HashMap.Strict as HM
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


-- _PARSE_NEW_ACCESS_TOKEN_AFTER = "var apiUrl = \"app/index.php?accessToken=\" + \""
_BASE_URL = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken="
_ACCESS_TOKEN = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiIxMDkuNjcuMTQ5LjE5NiIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NzExMDIwM30.S8N1uGmpXDWnvUUFMS8DWBw-qSlyxCJ_f5kQvFb50EM"
_URL_ID_LIST = _BASE_URL ++ _ACCESS_TOKEN ++ "&action=list"
_URL_HAND_ACTIONS = _BASE_URL ++ _ACCESS_TOKEN ++ "&action=detay&id="
_FILE_PATH_JSON = "database/hh-json/"
_FILE_PATH_CORRECTLY = _FILE_PATH_CORRECTLY_PROD
_FILE_PATH_CORRECTLY_PROD = "database/processed-correctly/" ++ _MONTH ++ "/"
_FILE_PATH_CORRECTLY_TEST = "database/hh-json/test/" ++ _TEST ++ "/"
_FILE_PATH_ERRORS = "database/processed-errors/"



_TEST = "test"

testHandIDData = C.pack "[{\"id\":\"592b343035a9080c68e768f6\",\"label\":\"28 (23:33)\",\"table\":\"Learning - Advanced 3 \"},{\"id\":\"592b33ee56a9083326fc42fa\",\"label\":\"28 (23:32)\",\"table\":\"Learning - Advanced 3 \"}]"

testPlayer = C.pack "{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}"
testHandActions = C.pack "{\"players\":[{\"id\":\"1728183\",\"name\":\"Eitan\",\"fid\":\"10153181502456109\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/24.png\",\"http://adv3.fbtrnpkr.info/kartlar/34.png\"],\"image\":\"https://graph.facebook.com/10153181502456109/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":100},{\"type\":\"RAISE\",\"chip\":\"3.23 K\"}]],\"chip\":\"19.76 K\",\"won\":true,\"onTheGame\":true},{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}],\"flop_0\":\"http://adv3.fbtrnpkr.info/kartlar/6.png\",\"flop_1\":\"http://adv3.fbtrnpkr.info/kartlar/45.png\",\"flop_2\":\"http://adv3.fbtrnpkr.info/kartlar/48.png\",\"turn\":\"http://adv3.fbtrnpkr.info/kartlar/0.png\",\"river\":\"http://adv3.fbtrnpkr.info/kartlar/37.png\"}"
chipAction = C.pack "{\"type\": \"BLIND\",\"chip\": \"50 K\"}"


reqLatest25HandIDsDebug = do
   r <- get _URL_ID_LIST
   return (parseEither parseHandIDs =<< eitherDecode (r ^. responseBody))



reqLatest25HandIDs :: IO (Maybe [HandID])
reqLatest25HandIDs = do
   r <- get _URL_ID_LIST
   return (parseMaybe parseHandIDs =<< decode (r ^. responseBody))

-- reqHandActions :: String -> IO (Either String HandActions)
reqHandActions hID = do
    r <- get (_URL_HAND_ACTIONS ++ hID)
    let decoded = (eitherDecode (r ^. responseBody) :: Either String HandActionsResp)
        fixed = fmap fixHandActions decoded
    return (decoded)

-- [(id, Date, fileName)]


reqHandActionsDebug hID = do
    r <- get (_URL_HAND_ACTIONS ++ hID)
    let decoded = (eitherDecode (r ^. responseBody) :: Either String HandActionsResp)
--         fixed = fmap fixHandActions decoded
    case decoded of
        (Left s) -> return ("dsfs " ++ s)
        (Right s) -> return "d"


readDebugHandJSONFileConvertWrite fileName = do
   let idDateFileName = (\f -> (parseTempFileNameID f, parseTempFileNameDate f, f)) fileName -- (id, simpledate)
   readHand idDateFileName

readJSONFileConvertWrite = do
   fileNs <- fmap (drop 1 . tail) (listDirectory (_FILE_PATH_JSON ++ _MONTH ++ "/" )) -- get IO list of json file from directory
   let idDateFileName = fmap (\f -> (parseTempFileNameID f, parseTempFileNameDate f, f)) fileNs -- (id, simpledate)
   mapM_ readHand idDateFileName


--
-- readFileUnsafe :: FilePath -> String
-- readFileUnsafe file = do
--     f <- readFile (_FILE_PATH_JSON ++ file



writeHandActionsUnparsedJSON :: HandID -> IO ()
writeHandActionsUnparsedJSON hID =
    let file = _FILE_PATH_JSON ++ (HandID.handID hID) ++(HandID.label (fixHandID hID)) ++  ".txt"
    in do
        n <- doesFileExist (file)
        case n of
            True -> return ()
            False -> do
                r <- get (_URL_HAND_ACTIONS ++ (HandID.handID hID))
                C.writeFile (file) (r ^. responseBody)

-- writeHandActions :: HandID -> IO ()
-- writeHandActions hID =
--     let file = _FILE_PATH_JSON ++ (HandID.handID hID) ++(HandID.label (fixHandID hID)) ++  ".txt"
--     in do
--         n <- doesFileExist (file)
--         case n of
--             True -> return ()
--             False -> do
--                 r <- get (_URL_HAND_ACTIONS ++ (HandID.handID hID))
--                 C.writeFile (file) (r ^. responseBody)

writeLatest25Hands = do
    r <-  reqLatest25HandIDs
    case r of
        (Just hIDs) -> (mapM_ writeHandActionsUnparsedJSON hIDs)
        _ -> return ()

-- readHand :: IO (Either String HandActions)
-- readHand :: IO (Either String [String])

readHand (iD, date, fileName) = do
    r <- fmap C.pack (readFile (_FILE_PATH_JSON ++ _MONTH ++ "/" ++ fileName))
    let decoded = (decode (r ) :: Maybe HandActionsResp)
        fixed = fmap fixHandActions decoded
        maybeHH = fmap (handToHH (iD, date)) fixed
        newFile = _FILE_PATH_CORRECTLY ++ iD ++ (show date) ++ ".txt"
    case maybeHH of
        Nothing -> return ()
        Just "too many players" -> return ()
        Just hh -> do
                n <- doesFileExist newFile
                case n of
                    True -> prependToFile newFile hh
                    False -> writeWithoutEsc newFile hh


readHandDebug (iD, date, fileName) = do
    r <- fmap C.pack (readFile (_FILE_PATH_JSON ++ _MONTH ++ "/" ++ fileName))
    let decoded = (decode (r ) :: Maybe HandActionsResp)
        fixed = fmap fixHandActions decoded
        maybeHH = fmap (handToHH (iD, date)) fixed
    case fixed of
        Nothing -> return ("nothing")
        Just (HandActions ps c1 c2 c3 c4 c5) -> return (show (  playerNames (getBlinds (head ps, last ps))))



writeWithoutEsc :: String -> String -> IO ()
writeWithoutEsc fileName contents = do
    handle <- openFile fileName WriteMode
    hPutStr handle contents
    hClose handle


-- handToHH :: HandActions -> String
handToHH  (hId, date) (HandActions ps c1 c2 c3 c4 c5) = do
    if length ps >= 3
        then "too many players"
    else let (sb, bb) = getBlinds (head ps, last ps)
             seat1 = head ps
             seat2 = last ps
--              pWinners = winners ps
             preFlopAction = getMergedStreetActions bb sb 0
             flopAction = getMergedStreetActions bb sb 1
             turnAction = getMergedStreetActions bb sb 2
             riverAction = getMergedStreetActions bb sb 3
             allAction = [preFlopAction, flopAction, turnAction, riverAction]
             pTotal = List.foldl (+) 0 (fmap potTotal allAction)
             seat1Stack = if div pTotal 2 > stack seat1 then div pTotal 2 else stack seat1
             seat2Stack = if div pTotal 2 > stack seat2 then div pTotal 2 else stack seat2
             s = [ "Winamax Poker - CashGame - HandId: #" ++ hId ++ " - Holdem no limit (" ++ show (quot (blind bb) 2) ++ "€/" ++ show (blind bb) ++ "€) - " ++ (showDateInsideHH date)
                 , "Table: " ++ (tableFromBB (blind bb)) ++ " 5-max (real money) Seat #" ++ (show (seat ps sb)) ++ " is the button"
                 , "Seat 1: " ++ (screenName seat1) ++ " (" ++ (show seat1Stack) ++ "€)"
                 , "Seat 2: " ++ (screenName seat2) ++ " (" ++ (show seat1Stack) ++ "€)"
                 , "*** ANTE/BLINDS ***"
                 , (screenName sb) ++ " posts " ++ (valToBlindName (blind sb)) ++ " blind " ++ (show (blind sb) ++ "€")
                 , (screenName bb) ++ " posts big blind " ++ (show (blind bb) ++ "€")
                 , "*** PRE-FLOP ***"
                 , "Dealt to " ++ (screenName (hero ps)) ++ " " ++ (hhCards (cards (hero ps)))
--                  , "PRE-FLOP:  " ++ (showMaybe (fmap showStreetActions preFlopAction))
                 , (showStreetActions preFlopAction [] 0)
                 , (showStreetActions flopAction [c1,c2,c3] 1)
                 , (showStreetActions turnAction [c1,c2,c3,c4] 2)
                 , (showStreetActions riverAction [c1,c2,c3,c4,c5] 3)
                 , if reachedShowDown [c1,c2,c3,c4,c5] then "" else (screenName (head (winners ps))) ++ " collected " ++ (show pTotal) ++ "€ from pot"
                 , "*** SUMMARY ***"
                 , "Total pot " ++ (show pTotal) ++ "€ | Rake 0€"
                 , showBoard [c1,c2,c3,c4,c5]
                 , showWinners ps pTotal
                 ]
         in removeExtraNewLines $ (concat (intersperse "\n" (List.filter (/= "") s))) ++ "\n"

valToBlindName :: Integer -> String
valToBlindName 50 = "small"
valToBlindName 100 = "big"
valToBlindName 20 = "small"
valToBlindName 40 = "big"
valToBlindName b = "ERROR!! blind is: " ++ show b

reachedShowDown :: [Maybe Card] -> Bool
reachedShowDown cs = length (catMaybes cs) == 5

removeExtraNewLines :: String -> String
removeExtraNewLines (x1:x2:xs) = if x1 == '\n' && x2 == '\n'
    then x2 : removeExtraNewLines xs
    else x1 : removeExtraNewLines (x2 : xs)
removeExtraNewLines (x1:[]) = x1:[]
removeExtraNewLines [] = []

showWinners :: [Player] -> Integer -> String
showWinners ps pTotal =
    let won = winners ps
        f = (\p ->
            "Seat " ++ (show (seat ps p)) ++ ": "
             ++ (screenName p) ++ " showed " ++ (hhCards (cards p)) ++
             " and won " ++ (show (div pTotal (toInteger (length won)))) ++ "€")
    in concat ( intersperse "\n" (fmap f won))


showBoard :: [Maybe Card] -> String
showBoard cards =
    if length (catMaybes cards) == 0
    then ""
    else "Board: " ++ (showListOfMaybes cards)


getMergedStreetActions :: Player -> Player -> Int -> Maybe [(String, ChipAction)]
getMergedStreetActions bb sb street =
    let sbActions = playersChipActionsOfStreet sb street
        bbActions = playersChipActionsOfStreet bb street
    in if street == 0
            then fmap (reverse . removeExtraStartingFolds . reverse ) $  mergeMaybeList sbActions bbActions
            else mergeMaybeList bbActions sbActions

showStreetActions Nothing _ _ = ""
showStreetActions (Just m) _ 0 = showStreetChipActions (drop 2 m)
showStreetActions (Just m) cards 1 = "*** FLOP *** " ++ (hhCards cards) ++ "\n" ++ (showStreetChipActions m)
showStreetActions (Just m) cards 2 = "*** TURN *** " ++ (hhCards cards) ++ "\n" ++ (showStreetChipActions m)
showStreetActions (Just m) cards 3 = "*** RIVER *** " ++ (hhCards cards) ++ "\n" ++ (showStreetChipActions m)
showStreetActions _ _ _ = ""


showListOfMaybes :: Show a => [Maybe a] -> String
showListOfMaybes mbs = "[" ++ (concat (intersperse " " ((fmap show) (catMaybes mbs)))) ++ "]"

showMaybe :: Show a => Maybe a -> String
showMaybe Nothing = ""
showMaybe (Just a) = show a

stringMaybe :: Maybe String -> String
stringMaybe Nothing = ""
stringMaybe (Just a) = a

showEither :: Either String String -> String
showEither (Left s) = s
showEither (Right a) = a

mergeMaybeList :: Maybe [a] -> Maybe [a] -> Maybe [a]
mergeMaybeList Nothing _ = Nothing
mergeMaybeList _ Nothing = Nothing
mergeMaybeList (Just l1) (Just l2) = Just $ merge l1 l2

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


prependToFile file str = do
    contents <- readFile file
    length contents `seq` (writeFile file $ (str ++ "\n\n\n\n\n\n\n\n" ++ contents))

main :: IO ()
main = writeLatest25Hands
-- main = do
--     h <- readHand
--     case h of
--         (Right hand) -> do
--             putStr hand
--         (Left e) -> do putStr e