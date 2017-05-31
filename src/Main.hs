{-# LANGUAGE OverloadedStrings #-}

module Main where

import Card
import ChipAction
import HandID
import Player
import HandActions
import GHC.Generics
import GHC.Exts as E (fromList)
import Data.Char (isDigit)
import Data.Text     as T                            (pack, unpack)
import qualified Data.Text.Lazy as TL            ( fromStrict, unpack )
import qualified Data.ByteString.Lazy as BSL     ( toChunks , pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as C ( fromChunks, pack)
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.HashMap.Strict as HM
-- import qualified Data.Text.Lazy.IO as T
-- import qualified Data.Text.Lazy.Encoding as T
-- import qualified Data.Text.Lazy as T (unpack)
import Network.Wreq
import Control.Lens
import Prelude hiding (putStrLn)
import Data.Map as Map
import qualified Data.Vector as V
import Data.List as List
import Data.Aeson
import Data.Aeson.Types as Ty
import Data.Aeson.Lens (_String, key, nth, _Array)
import Control.Applicative
import Control.Monad



-- TODO update automaticaly to new url
_URL_ID_LIST = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjE3NjE3N30.2XnBNtXhjZwnTv3ifo8cQJjkLyVfh_s06xIXCQDUjgs&action=list"
_SAMPLE_ID = "592d5d67661c65f81004cff5"
_sdf =   "https://www.turngs.com/games/poker/plugins/log/index.php?accessToken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjI2MzQ4NX0.UDf-qk89osAFRmslo3iYEKusyYUQvNkqtE8vV80m-GI#"
_URL_HAND_ACTIONS = "https://www.turngs.com/games/poker/plugins/log/app/index.php?accessToken=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6MTcyODE4MywiaXAiOiI3OS4xODAuMjkuNCIsImZ1bGxBY2Nlc3MiOnRydWUsImV4cCI6MTQ5NjI2MzQ4NX0.UDf-qk89osAFRmslo3iYEKusyYUQvNkqtE8vV80m-GI&action=detay&id="




testHandIDData = C.pack "[{\"id\":\"592b343035a9080c68e768f6\",\"label\":\"28 (23:33)\",\"table\":\"Learning - Advanced 3 \"},{\"id\":\"592b33ee56a9083326fc42fa\",\"label\":\"28 (23:32)\",\"table\":\"Learning - Advanced 3 \"}]"

testPlayer = C.pack "{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}"
testHandActions = C.pack "{\"players\":[{\"id\":\"1728183\",\"name\":\"Eitan\",\"fid\":\"10153181502456109\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/24.png\",\"http://adv3.fbtrnpkr.info/kartlar/34.png\"],\"image\":\"https://graph.facebook.com/10153181502456109/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":100},{\"type\":\"RAISE\",\"chip\":\"3.23 K\"}]],\"chip\":\"19.76 K\",\"won\":true,\"onTheGame\":true},{\"id\":\"2907539\",\"name\":\"Gusdi Darmawan\",\"fid\":\"676527835866370\",\"cards\":[\"http://adv3.fbtrnpkr.info/kartlar/51.png\",\"http://adv3.fbtrnpkr.info/kartlar/18.png\"],\"image\":\"https://graph.facebook.com/676527835866370/picture\",\"actions\":[[{\"type\":\"BLIND\",\"chip\":50},{\"type\":\"CALL\",\"chip\":100},{\"type\":\"CALL\",\"chip\":\"3.23 K\"}]],\"chip\":\"3.23 K\",\"won\":false,\"onTheGame\":true}],\"flop_0\":\"http://adv3.fbtrnpkr.info/kartlar/6.png\",\"flop_1\":\"http://adv3.fbtrnpkr.info/kartlar/45.png\",\"flop_2\":\"http://adv3.fbtrnpkr.info/kartlar/48.png\",\"turn\":\"http://adv3.fbtrnpkr.info/kartlar/0.png\",\"river\":\"http://adv3.fbtrnpkr.info/kartlar/37.png\"}"
chipAction = C.pack "{\"type\": \"BLIND\",\"chip\": \"50 K\"}"


reqLatest25HandIDs :: IO (Maybe [HandID])
reqLatest25HandIDs = do
   r <- get _URL_ID_LIST
   return (parseMaybe parseHandIDs =<< decode (r ^. responseBody))

reqHandActions :: String -> IO (Either String HandActions)
reqHandActions hID = do
    r <- get (_URL_HAND_ACTIONS ++ hID)
    let decoded = (eitherDecode (r ^. responseBody) :: Either String HandActionsResp)
        fixed = fmap fixHandActions decoded
    return (fixed)

main :: IO ()
main = undefined