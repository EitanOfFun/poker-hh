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
  } deriving (Show)

instance FromJSON HandID where
   parseJSON (Object v) =
    HandID <$> v .: "id"
           <*> v .: "label"
           <*> v .: "table"
   parseJSON _ = mzero

parseHandIDs :: Value -> Parser [HandID]
parseHandIDs = withObject "HandIDs" $ \o -> do
  o .: "data"

