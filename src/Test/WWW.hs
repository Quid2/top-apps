module Test.WWW where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Network.HTTP.Conduit (Request, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpBS)
import Test.Types

wwwTest :: (String, B8.ByteString) -> Test
wwwTest = wwwTest_ contains

wwwTest_ cond (url, key) = test url (cond key) (parseRequest url >>= getURL)

notNull :: B8.ByteString -> Maybe String
notNull s = if B8.null s then Just "No content" else Nothing

-- contains "Flat"
contains :: B8.ByteString -> B8.ByteString -> Maybe String
contains k s = if k `B8.isInfixOf` s then Nothing else Just (unwords ["does not contain", show k])

notContains :: B8.ByteString -> B8.ByteString -> Maybe String
notContains k s = if not (k `B8.isInfixOf` s) then Nothing else Just (unwords ["does contain", show k])

getURL :: Request -> IO B8.ByteString
getURL url = getResponseBody <$> httpBS url
