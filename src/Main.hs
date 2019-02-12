{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Card                       (Card, hhCards)
import           ChipAction                 (ChipAction, potTotal,
                                             removeExtraStartingFolds,
                                             showStreetChipActions)
import           Control.Lens               ((^.))
import           Control.Monad              (unless)
import           Data.Aeson                 (decode, eitherDecode)
import           Data.Aeson.Types           (parseMaybe)
import qualified Data.ByteString.Lazy       as BSL (pack, toChunks, unpack)
import qualified Data.ByteString.Lazy.Char8 as C (fromChunks, pack, readFile,
                                                  writeFile)
import           Data.Char                  (isDigit)
import qualified Data.HashMap.Lazy          as HML (lookup)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (delete, filter, foldl, intercalate,
                                             intersperse)
import           Data.Maybe
import qualified Data.Text                  as T (pack, unpack)
import qualified Data.Text.Lazy             as TL (fromStrict, unpack)
import qualified GHC.Exts                   as E (fromList)
import           HandActions                (HandActions (..),
                                             HandActionsResp (..),
                                             fixHandActions)
import           HandID                     (HandID, fixHandID, handID, label,
                                             parseHandIDs, parseTempFileNameID,
                                             tableFromBB)
import           Network.Wreq               (get, responseBody)
import           Player                     (Player, blind, cards, cardsKnown,
                                             getBlinds, hero,
                                             playersChipActionsOfStreet,
                                             screenName, seat, sortByWinner,
                                             stack, winners, won, _HERO_ID)
import           Prelude                    hiding (putStrLn)
import           System.Directory           (doesFileExist, listDirectory)
import           System.IO                  (IOMode (WriteMode), hClose,
                                             hPutStr, openFile)
import           TurnDate                   (TurnDate, getCurrentDate,
                                             parseTempFileNameDate,
                                             showDateInsideHH,
                                             showDateOutsideHH)
import           Utils                      (mapTuple, merge, mergeMaybeList,
                                             showEither, showListOfMaybes,
                                             showMaybe, stringMaybe)

_BASE_URL = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken="

_FILE_PATH_JSON = "database/hh-json/"

_FILE_PATH_WINAMAX = "database/winamax/"

_FILE_PATH_ERRORS = "database/processed-errors/"

_FILE_PATH_ACCESS_TOKEN = "database/access_token.txt"

getAccessToken :: IO String
getAccessToken = readFile _FILE_PATH_ACCESS_TOKEN

reqLatest25HandIDs :: String -> IO (Maybe [HandID])
reqLatest25HandIDs accessToken = do
    r <- get (_BASE_URL ++ accessToken ++ "&action=list")
    return (parseMaybe parseHandIDs =<< decode (r ^. responseBody))

reqHandActions accessToken hID = do
    r <- get (_BASE_URL ++ accessToken ++ "&action=detay&id=" ++ hID)
    let decoded = eitherDecode (r ^. responseBody) :: Either String HandActionsResp
        fixed = fmap fixHandActions decoded
    return decoded

writeHandActions :: String -> HandID -> IO ()
writeHandActions accessToken hID = do
    (year, month, day) <- getCurrentDate
    let rawJSONfile =
            _FILE_PATH_JSON ++
            show year ++
            "/" ++ show month ++ "/" ++ HandID.handID hID ++ HandID.label (fixHandID hID) ++ ".txt"
        idDateFileName = (HandID.handID hID, parseTempFileNameDate (year, month, day) rawJSONfile)
    n <- doesFileExist rawJSONfile
    unless n $ do
        r <- get $ _BASE_URL ++ accessToken ++ "&action=detay&id=" ++ HandID.handID hID
        C.writeFile rawJSONfile (r ^. responseBody)
        readHandNew idDateFileName (r ^. responseBody)

readHandNew (iD, date) r = do
    let decoded = decode r :: Maybe HandActionsResp
        fixed = fmap fixHandActions decoded
        maybeHH = fmap (handToHH (iD, date)) fixed
    case maybeHH of
        Nothing -> return ()
        Just "too many players" -> return ()
        Just hh -> do
            (year, month, _) <- getCurrentDate
            let newFile =
                    _FILE_PATH_WINAMAX ++
                    show year ++ "/" ++ show month ++ "/" ++ iD ++ showDateOutsideHH date ++ ".txt"
            n <- doesFileExist newFile
            if n
                then return ()
                else writeWithoutEsc newFile hh

writeWithoutEsc :: String -> String -> IO ()
writeWithoutEsc fileName contents = do
    handle <- openFile fileName WriteMode
    hPutStr handle contents
    hClose handle

handToHH (hId, date) (HandActions ps c1 c2 c3 c4 c5) =
    if length ps >= 3
        then "too many players"
        else let (sb, bb) = getBlinds ps
                 seat1 = head ps
                 seat2 = ps !! 1
                 preFlopAction = getMergedStreetActions bb sb 0
                 flopAction = getMergedStreetActions bb sb 1
                 turnAction = getMergedStreetActions bb sb 2
                 riverAction = getMergedStreetActions bb sb 3
                 allAction = [preFlopAction, flopAction, turnAction, riverAction]
                 pTotal = sum (fmap potTotal allAction)
                 seat1Stack =
                     if div pTotal 2 > stack seat1
                         then div pTotal 2
                         else stack seat1
                 seat2Stack =
                     if div pTotal 2 > stack seat2
                         then div pTotal 2
                         else stack seat2
                 s =
                     [ "Winamax Poker - CashGame - HandId: #" ++
                       hId ++
                       " - Holdem no limit (" ++
                       show (quot (blind bb) 2) ++ "€/" ++ show (blind bb) ++ "€) - " ++ showDateInsideHH date
                     , "Table: " ++
                       tableFromBB (blind bb) ++
                       " 5-max (real money) Seat #" ++ show (seat ps sb) ++ " is the button"
                     , "Seat 1: " ++ screenName seat1 ++ " (" ++ show seat1Stack ++ "€)"
                     , "Seat 2: " ++ screenName seat2 ++ " (" ++ show seat2Stack ++ "€)"
                     , "*** ANTE/BLINDS ***"
                     , screenName sb ++
                       " posts " ++ valToBlindName (blind sb) ++ " blind " ++ show (blind sb) ++ "€"
                     , screenName bb ++ " posts big blind " ++ show (blind bb) ++ "€"
                     , "*** PRE-FLOP ***"
                     , "Dealt to " ++
                       screenName (hero _HERO_ID ps) ++ " " ++ hhCards (cards (hero _HERO_ID ps))
                     , showStreetActions preFlopAction [] 0
                     , showStreetActions flopAction [c1, c2, c3] 1
                     , showStreetActions turnAction [c1, c2, c3, c4] 2
                     , showStreetActions riverAction [c1, c2, c3, c4, c5] 3
                     , if reachedShowDown [c1, c2, c3, c4, c5]
                           then ""
                           else screenName (head (winners ps)) ++ " collected " ++ show pTotal ++ "€ from pot"
                     , "*** SUMMARY ***"
                     , "Total pot " ++ show pTotal ++ "€ | No rake"
                     , showBoard [c1, c2, c3, c4, c5]
                     , showWinners ps pTotal
                     ]
             in removeExtraNewLines $ intercalate "\n" (filter (/= "") s) ++ "\n"

valToBlindName :: Integer -> String
valToBlindName 50  = "small"
valToBlindName 100 = "big"
valToBlindName 200 = "big" -- TODO
valToBlindName 20  = "small"
valToBlindName 40  = "big"
valToBlindName b   = "ERROR!! blind is: " ++ show b

reachedShowDown :: [Maybe Card] -> Bool
reachedShowDown cs = length (catMaybes cs) == 5

removeExtraNewLines :: String -> String
removeExtraNewLines (x1:x2:xs) =
    if x1 == '\n' && x2 == '\n'
        then x2 : removeExtraNewLines xs
        else x1 : removeExtraNewLines (x2 : xs)
removeExtraNewLines [x] = [x]
removeExtraNewLines [] = []

showWinners :: [Player] -> Integer -> String
showWinners ps pTotal =
    let sorted = sortByWinner ps
        f p =
            if not (cardsKnown (cards p))
                then if won p
                         then "Seat " ++
                              show (seat ps p) ++ ": " ++ screenName p ++ " won " ++ show pTotal ++ "€"
                         else ""
                else "Seat " ++
                     show (seat ps p) ++
                     ": " ++
                     screenName p ++
                     " showed " ++
                     hhCards (cards p) ++
                     if won p
                         then " and won " ++ show (div pTotal (toInteger (length (winners sorted)))) ++ "€"
                         else ""
    in intercalate "\n" (filter (/= "") (map f sorted))

showBoard :: [Maybe Card] -> String
showBoard cards =
    if null (catMaybes cards)
        then ""
        else "Board: " ++ showListOfMaybes cards

getMergedStreetActions :: Player -> Player -> Int -> Maybe [(String, ChipAction)]
getMergedStreetActions bb sb street =
    let sbActions = playersChipActionsOfStreet sb street
        bbActions = playersChipActionsOfStreet bb street
    in if street == 0
           then (reverse . removeExtraStartingFolds . reverse) <$> mergeMaybeList sbActions bbActions
           else mergeMaybeList bbActions sbActions

showStreetActions :: Maybe [(String, ChipAction)] -> [Maybe Card] -> Int -> String
showStreetActions Nothing cards 1 =
    if isJust $ last cards
        then "*** FLOP *** " ++ hhCards cards ++ "\n"
        else ""
showStreetActions Nothing cards 2 =
    if isJust $ last cards
        then "*** TURN *** " ++ hhCards cards ++ "\n"
        else ""
showStreetActions Nothing cards 3 =
    if isJust $ last cards
        then "*** RIVER *** " ++ hhCards cards ++ "\n"
        else ""
showStreetActions Nothing _ _ = ""
showStreetActions (Just m) _ 0 = showStreetChipActions (drop 2 m)
showStreetActions (Just m) cards 1 = "*** FLOP *** " ++ hhCards cards ++ "\n" ++ showStreetChipActions m
showStreetActions (Just m) cards 2 = "*** TURN *** " ++ hhCards cards ++ "\n" ++ showStreetChipActions m
showStreetActions (Just m) cards 3 = "*** RIVER *** " ++ hhCards cards ++ "\n" ++ showStreetChipActions m
showStreetActions _ _ _ = ""

prependToFile :: FilePath -> String -> IO ()
prependToFile file str = do
    contents <- readFile file
    length contents `seq` writeFile file (str ++ "\n\n\n\n\n\n\n\n" ++ contents)

writeLatest25HandsNew :: IO ()
writeLatest25HandsNew = do
    accessToken <- getAccessToken
    r <- reqLatest25HandIDs accessToken
    case r of
        (Just hIDs) -> mapM_ (writeHandActions accessToken) hIDs
        _           -> return ()

main :: IO ()
main = writeLatest25HandsNew
