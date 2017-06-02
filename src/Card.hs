{-# LANGUAGE OverloadedStrings #-}

module Card where

import Data.Char (isDigit)
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
-- exampleCardUrl = C.pack "http://adv3.fbtrnpkr.info/kartlar/0.png"


hhCards :: [Maybe Card] -> String
hhCards ((Just c1):(Just c2):[]) = "[" ++ (show c1) ++ " " ++ (show c2) ++ "]"
hhCards ((Just c1):(Just c2):(Just c3):[]) = "[" ++ (show c1) ++ " " ++ (show c2) ++ " " ++ (show c3) ++ "]"
hhCards ((Just c1):(Just c2):(Just c3):(Just c4):[]) = "["++(show c1)++" "++(show c2)++" "++(show c3)++"]"++" ["++(show c4)++"]"
hhCards ((Just c1):(Just c2):(Just c3):(Just c4):(Just c5):[]) = "["++(show c1)++" "++(show c2)++" "++(show c3)++" "++(show c4)++"]"++" ["++(show c5)++"]"
hhCards _ = "[]"