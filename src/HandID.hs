{-# LANGUAGE OverloadedStrings #-}

module HandID where

import Data.Aeson
import Data.Aeson.Types as Ty
import Control.Applicative
import Control.Monad


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



tableFromBB :: Integer -> String
tableFromBB 100 = "'BigDog-Advanced1'"
tableFromBB 40 = "'SuperWhales-Advanced2'"
tableFromBB _ = "'lowStakes'"