module Utils
    ( showListOfMaybes
    , showMaybe
    , showEither
    , mergeMaybeList
    , stringMaybe
    , merge
    , mapTuple
    , filterTuple
    , toTuple
    ) where

import           Data.Maybe (catMaybes)

showListOfMaybes :: Show a => [Maybe a] -> String
showListOfMaybes mbs = "[" ++ unwords (fmap show (catMaybes mbs)) ++ "]"

showMaybe :: Show a => Maybe a -> String
showMaybe Nothing  = ""
showMaybe (Just a) = show a

stringMaybe :: Maybe String -> String
stringMaybe Nothing  = ""
stringMaybe (Just a) = a

showEither :: Either String String -> String
showEither (Left s)  = s
showEither (Right a) = a

mergeMaybeList :: Maybe [a] -> Maybe [a] -> Maybe [a]
mergeMaybeList (Just l1) (Just l2) = Just $ merge l1 l2
mergeMaybeList _ _                 = Nothing

merge :: [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

filterTuple :: (a -> Bool) -> (a, a) -> [a]
filterTuple f (a1, a2) = catMaybes [true f a1, true f a2]

true :: (a -> Bool) -> a -> Maybe a
true f a
    | f a = Just a
    | otherwise = Nothing

toTuple :: [a] -> (a, a)
toTuple [p1, p2] = (p1, p2)
toTuple _        = error "List doesn't have exactly two elements! List: "
