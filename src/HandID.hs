{-# LANGUAGE OverloadedStrings #-}

module HandID where

import Data.Aeson
import Data.Aeson.Types as Ty
import Control.Applicative
import Control.Monad

_YEAR = "2017"
_MONTH = "06"

data HandID = HandID
  { handID :: !String
  , label :: !String
  , table :: !String
  } deriving (Show, Eq)

instance FromJSON HandID where
   parseJSON (Object v) =
    HandID <$> v .: "id"
           <*> v .: "label"
           <*> v .: "table"
   parseJSON _ = mzero

parseHandIDs :: Value -> Parser [HandID]
parseHandIDs = withObject "HandIDs" $ \o -> do
  o .: "data"

fixHandID :: HandID -> HandID
fixHandID (HandID h1 h2 h3) = HandID {
    handID = h1
  , label = "_" ++ (fmap replaceBadCharWith_ h2)
  , table = h3
}

replaceBadCharWith_ ' ' = '_'
replaceBadCharWith_ '(' = '_'
replaceBadCharWith_ ')' = '_'
replaceBadCharWith_ ':' = '_'
replaceBadCharWith_ c = c

parseTempFileNameID :: String -> String
parseTempFileNameID = takeWhile ((/=) '_')

parseTempFileNameDate :: String -> String
parseTempFileNameDate fileName =
    let hID = parseTempFileNameID fileName
        t = drop 1 (dropWhile ((/=) '_') fileName)
        day = take 2 t
        hour = take 2 (drop 4 t)
        min = take 2 (drop 7 t)
        sec = hexToDec (last hID)
    in _YEAR ++ "/" ++ _MONTH ++ "/" ++ day ++ " " ++ hour ++ ":" ++ min ++ ":" ++ sec ++ " UTC"

hexToDec 'f' = "15"
hexToDec 'e' = "14"
hexToDec 'd' = "13"
hexToDec 'c' = "12"
hexToDec 'b' = "11"
hexToDec 'a' = "10"
hexToDec c = '0':c:[]
-- date :: HandID

tableFromBB :: Integer -> String
tableFromBB 100 = "'BigDog-Advanced1'"
tableFromBB 40 = "'SuperWhales-Advanced2'"
tableFromBB _ = "'lowStakes'"