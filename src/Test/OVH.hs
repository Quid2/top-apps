{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.OVH where

import           Data.Aeson
import           Data.String          (IsString (fromString))
import           Test.WWW             (getURL)

import           Data.ByteString.Lazy (fromStrict)
import           Data.Maybe
import           Data.Text            (Text)
import           GHC.Generics
import           Network.HTTP.Conduit (Request, parseRequest)
import           Test.Types
import           Test.WWW
import           Text.Show.Pretty     (pPrint)

{-
Kimsufi:
Name Code
KS-1 2201sk010
KS-2 2201sk011
KS-3 2201sk012
KS-17 2201sk081
-}
ovhTest srv = Test ("Test OVH server " ++ srv) 30 (parseRequest (ksURL srv) >>= getURL) (availableServer srv)

-- ovhTest srv = wwwTest (availableServer srv)

availableServer srv r =
    if isAvailable . fromJust $ (decode (fromStrict r) :: Maybe [Region]) then Just (unwords ["server",srv,"is available"]) else Nothing

-- k :: IO (Maybe Value)
k :: IO ()
k = do
    Just srv <-  ks_ "2201sk080" -- "2201sk010" --
    pPrint srv
    print $ isAvailable srv
    -- pPrint (decode r :: Maybe [Region])

ks :: String -> IO Bool
ks srv = do
    Just rs <- ks_ srv
    return $ isAvailable rs


ks_ :: String -> IO (Maybe [Region])
ks_ srv = do
    r <- getURL $ fromString $ "https://api.ovh.com/1.0/dedicated/server/availabilities?country=IT&hardware="++srv
    return $ decode (fromStrict r)

ksURL srv = fromString $ "https://api.ovh.com/1.0/dedicated/server/availabilities?country=IT&hardware="++srv


isAvailable :: [Region] -> Bool
isAvailable = any ( (/= "unavailable") . availability) . datacenters . head . filter ((== "europe") . region)

-- getAvailabilities = id

-- isEurope ds = withObject "datacenters" $ \o -> o .: "region"

data Datacenter = Datacenter {
      availability :: Text
    , datacenter   :: Text
    } deriving (Generic, Show)

instance ToJSON Datacenter where toEncoding = genericToEncoding defaultOptions

instance FromJSON Datacenter

data Region = Region {
      datacenters :: [Datacenter]
    , hardware    :: Text
    , region      :: Text
    } deriving (Generic, Show)

instance ToJSON Region where toEncoding = genericToEncoding defaultOptions

instance FromJSON Region
