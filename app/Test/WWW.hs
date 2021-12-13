module Test.WWW where
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as M
import           Network.HTTP.Conduit  (Request, parseRequest)
import           Network.HTTP.Simple   (getResponseBody, httpBS)
import           Test.Types            (Check, Test (Test))


wwwTest :: (String, B8.ByteString) -> Test
wwwTest (url,key) = Test ("Test " ++ url) 30 (parseRequest url >>= getURL) (contains key) -- (const Nothing) --

notNull :: Check
notNull s = if B8.null s then Just "No content" else Nothing

-- contains "Flat"
contains :: B8.ByteString -> B8.ByteString -> Maybe String
contains k s = if k `B8.isInfixOf` s then Nothing else Just (unwords ["does not contain",show k])

getURL :: Request -> IO B8.ByteString
getURL url = getResponseBody <$> httpBS url

