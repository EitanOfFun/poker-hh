{-# LANGUAGE OverloadedStrings #-}

module ChipAction where

import Data.Aeson
import Data.Aeson.Types as Ty
import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as HML        ( lookup )
import Data.List as L

data ChipAction =
    Blind Integer
  | Allin Integer
  | Raise Integer
  | Call Integer
  | Check
  | Fold
  deriving (Eq, Show)

showStreetChipActions :: [(String, ChipAction)] -> String
showStreetChipActions xs = foldl (\str tuple -> str ++ (showChipActionWithScreenName tuple) ++ "\n") "" xs

showChipActionWithScreenName :: (String, ChipAction) -> String
showChipActionWithScreenName (s, action) = s ++ " " ++ (showChipAction action)

showChipAction (Blind 50) = "posts the small blind of $50"
showChipAction (Blind 100) = "posts the big blind of $100"
showChipAction (Blind 200) = "posts the small blind of $20"
showChipAction (Blind 40) = "posts the big blind of $40"
showChipAction (Allin n) = "raises to $" ++ (show n)
showChipAction (Raise n) = "raises to $" ++ (show n)
showChipAction (Call n) = "calls $" ++ (show n)
showChipAction Check = "checks"
showChipAction Fold = "folds"
showChipAction (Blind b) = "ERROR: Blind " ++ (show b) ++ " not supported"



parseChipActions :: Maybe Value -> Parser [ChipAction]
parseChipActions (Just (Array arr)) = mapM parseJSON (V.toList arr)
parseChipActions _           = fail "expected an array of ChipActions"


parseChipString :: String -> Integer
parseChipString "" = 0
parseChipString s =
    let amt = init (init s) in
    case last s of
      'B' -> floor $ (read amt :: Double) * 1000
      'M' -> floor $ (read amt :: Double)
      _   -> 1

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


-- postFlopAction :: [ChipAction] -> [(String, ChipAction)]
-- postFlopAction ps 0 = fixPreflopBug


fixCalls :: [ChipAction] -> [ChipAction]
fixCalls [] = []
fixCalls (x1: (Call c) : xs) = case x1 of
    (Blind b) -> case (c - b) of
        0 -> x1 : Check : (fixCalls xs)
        d -> x1 : (Call d): (fixCalls xs)
--     (Raise b) -> case (c - b) of   // don't know why I needed this case (doesn't make sense to me now)
--         0 -> x1 : Check : (fixCalls xs)
--         d -> x1 : (Call d): (fixCalls xs)
    _ -> x1 : (Call c): (fixCalls xs)
fixCalls (x1 : x2) = x1 : fixCalls x2
