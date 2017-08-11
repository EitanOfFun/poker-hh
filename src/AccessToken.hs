module AccessToken where

data AccessToken = AccessToken String

instance FromJSON AccessToken where
   parseJSON (Object v) = AccessToken <$> v .: "token"
   parseJSON _ = mzero
