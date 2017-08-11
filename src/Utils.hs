module Utils where

import Data.Maybe
import Data.List (intersperse)


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