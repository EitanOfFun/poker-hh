{-# LANGUAGE OverloadedStrings #-}

module HandID where

import           Control.Monad    (mzero)
import           Data.Aeson       (FromJSON, parseJSON)
import           Data.Aeson.Types (Object, Parser, Value (..), withObject, (.:))


data HandID = HandID
  { handID :: !String
  , label  :: !String
  } deriving (Show, Eq)

instance FromJSON HandID where
   parseJSON (Object v) =
    HandID <$> v .: "id"
           <*> v .: "label"
   parseJSON _ = mzero

parseHandIDs :: Value -> Parser [HandID]
parseHandIDs = withObject "HandIDs" $ \o ->
  o .: "data"

fixHandID :: HandID -> HandID
fixHandID (HandID h1 h2) = HandID {
    handID = h1
  , label = "_" ++ fmap replaceBadCharWith_ h2
}

replaceBadCharWith_ ' ' = '_'
replaceBadCharWith_ '(' = '_'
replaceBadCharWith_ ')' = '_'
replaceBadCharWith_ ':' = '_'
replaceBadCharWith_ c   = c

parseTempFileNameID :: FilePath -> String
parseTempFileNameID = takeWhile ('_' /=)

--
--
tableFromBB :: Integer -> String
tableFromBB 100 = "'BigDog-Advanced1'"
tableFromBB 40  = "'SuperWhales-Advanced2'"
tableFromBB _   = "'lowStakes'"
