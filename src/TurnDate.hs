module TurnDate where

import Data.Time.Calendar
import           HandID  (parseTempFileNameID)
import           Prelude hiding (min)
import           Data.Time.Clock (getCurrentTime, utctDay)
import           Data.Time.Calendar (toGregorian)


data TurnDate = TurnDate
    { year  :: !String
    , month :: !String
    , day   :: !String
    , hour  :: !String
    , min   :: !String
    , sec   :: !String
    }


instance Show TurnDate where
    show = showDateOutsideHH

showDateInsideHH :: TurnDate -> String
showDateInsideHH (TurnDate y mo d h mi s) = y ++ "/" ++ mo ++ "/" ++ d ++ " " ++ h ++ ":" ++ mi ++ ":" ++ s ++ " UTC"

showDateOutsideHH :: TurnDate -> String
showDateOutsideHH (TurnDate y mo d _ _ _) = "-" ++ mo ++ "-" ++ d ++ "-" ++ y

parseTempFileNameDate :: (Integer,Int,Int) -> FilePath -> TurnDate
parseTempFileNameDate (year_, month_, day_) fileName =
    let hID = parseTempFileNameID fileName
        t = drop 1 (dropWhile ('_' /=) fileName)
    in TurnDate
        { year = show year_
        , month = show month_
        , day = take 2 t
        , hour = take 2 (drop 4 t)
        , min = take 2 (drop 7 t)
        , sec = hexToDec (last hID)
        }

hexToDec :: Char -> String
hexToDec 'f' = "15"
hexToDec 'e' = "14"
hexToDec 'd' = "13"
hexToDec 'c' = "12"
hexToDec 'b' = "11"
hexToDec 'a' = "10"
hexToDec c   = ['0', c]

getCurrentDate :: IO (Integer,Int,Int) -- (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay
