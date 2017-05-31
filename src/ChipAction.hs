{-# LANGUAGE OverloadedStrings #-}

module ChipAction where

import Data.Aeson
import Data.Aeson.Types as Ty
import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HML        ( lookup )

data ChipAction =
    Blind Integer
  | Allin Integer
  | Raise Integer
  | Call Integer
  | Check
  | Fold
  deriving (Eq, Show)


parseChipActions :: Maybe Value -> Parser [ChipAction]
parseChipActions (Just (Array arr)) = mapM parseJSON (V.toList arr)
parseChipActions _           = fail "expected an array of ChipActions"


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
      Just (String "RAISE") -> ((Raise . parseChipString . showInt) <$> o .: "chip" <|> (Raise . parseChipString <$> o .: "chip"))
      Just (String "ALLIN") -> ((Allin . parseChipString . showInt) <$> o .: "chip" <|> (Allin . parseChipString <$> o .: "chip"))
      Just (String "CALL") -> ((Call . parseChipString . showInt) <$> o .: "chip" <|> (Call . parseChipString <$> o .: "chip"))
      Just (String "CHECK") -> return Check
      Just (String "FOLD") -> return ChipAction.Fold
  parseJSON _ = mzero


