module TurnDate where

import           HandID  (parseTempFileNameID)
import           Prelude hiding (min)

-- TODO:  update these when finished processing and figure out how to automate it
_YEAR = "2017"
_MONTH = "08"

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
showDateOutsideHH (TurnDate y mo d h mi s) = "-" ++ mo ++ "-" ++ d ++ "-" ++ y

parseTempFileNameDate :: FilePath -> TurnDate
parseTempFileNameDate fileName =
    let hID = parseTempFileNameID fileName
        t = drop 1 (dropWhile ('_' /=) fileName)
    in TurnDate
        { year = _YEAR
        , month = _MONTH
        , day = take 2 t
        , hour = take 2 (drop 4 t)
        , min = take 2 (drop 7 t)
        , sec = hexToDec (last hID)
        }

hexToDec 'f' = "15"
hexToDec 'e' = "14"
hexToDec 'd' = "13"
hexToDec 'c' = "12"
hexToDec 'b' = "11"
hexToDec 'a' = "10"
hexToDec c   = ['0', c]
