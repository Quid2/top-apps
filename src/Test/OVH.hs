{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.OVH where

import           Data.Aeson           (FromJSON, ToJSON (toEncoding), decode,
                                       defaultOptions, genericToEncoding)
import           Data.ByteString.Lazy (fromStrict)
import           Data.Maybe           (fromJust)
import           Data.String          (IsString (fromString))
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Network.HTTP.Conduit (Request, parseRequest)
import           Test.Types           (ByteString, Test (Test))
import           Test.WWW             (getURL)
import           Text.Show.Pretty     (pPrint)

servers = [("KS-1","2201sk010"),("KS-2","2201sk011"),("KS-3","2201sk012"),("KS-16","2201sk080"),("KS-17","2201sk081")]

ovhTest :: String -> Test
ovhTest srvName = let Just srv = lookup srvName servers in Test ("Test OVH server " ++ srvName) 30 (parseRequest (ksURL srv) >>= getURL) (availableServer srv)

availableServer :: String -> ByteString -> Maybe String
availableServer srv r =
    if isAvailable . fromJust $ (decode (fromStrict r) :: Maybe [Region]) then Just "server is available" else Nothing

ksURL :: IsString a => [Char] -> a
ksURL srv = fromString $ "https://api.ovh.com/1.0/dedicated/server/availabilities?country=it&hardware="++srv

isAvailable :: [Region] -> Bool
isAvailable = any ( (/= "unavailable") . availability) . concatMap datacenters  . filter ((`elem` ["europe","northAmerica"]) . region)

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

-- Test

k :: IO ()
k = do
    let srv = "2201sk081" -- "2201sk010" --
    Just rs <-   ks_ srv
    let rs' = filter ((== fromString srv) . hardware) rs
    pPrint rs'
    print $ isAvailable rs'
    -- pPrint (decode r :: Maybe [Region])

ks_ :: String -> IO (Maybe [Region])
ks_ srv = do
    r <- getURL $ ksURL srv
    return $ decode (fromStrict r)
