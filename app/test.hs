module Test where

import Control.Concurrent.Async
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Data.Time.Util
import Network.HaskellNet.SMTP.SSL
import Network.Top
import Repo.Memory
import System.Environment
import System.Timeout

t = mapM (test . HTTPTest notNull) ["http://quid2.org", "https://quid2.org", "https://quid2.org/app/ui", "http://kamus.it"]

notNull :: Check
notNull s = if B8.null s then Just "No content" else Nothing

class AsTest t where test :: t -> IO (Maybe String)

type Check = B8.ByteString -> Maybe String

-- data HTTPTest = HTTPTest {check :: Check, req :: Request}

instance Test HTTPTest where
    test t = do
        er <- getURL (req t)
        return $ case er of
            Left e -> Just (show e)
            Right c -> check t c

getURL :: Request -> IO (Either HttpException B8.ByteString)
getURL url = try $ getResponseBody <$> httpBS url

data Test = Test
    { name :: String
    , timeoutInSecs :: Int
    , source :: IO (Either SomeException B8.ByteString)
    , check :: Check
    }

runTests = mapM runTest

runTest :: Test -> IO (String, Maybe String)
runTest t =
    async (timeout (seconds $ timeoutInSecs t) (op t))
        >>= ((name t,) . chk <$>)
            . waitCatch

chk (Right Nothing) = Just "Test timeout"
chk (Right (Just False)) = Just "Wrong Test Result"
chk (Right (Just True)) = Nothing
chk (Left exp) = Just (show exp)

e = email "Trying" "Nothing to see here" "quidagent"

email :: String -> String -> String -> String -> IO ()
email title body fromGmail fromGmailPwd = do
    print $ unwords ["EMAIL", title, body]
    doSMTPSTARTTLSWithSettings
        "smtp.gmail.com"
        defaultSettingsSMTPSTARTTLS{sslLogToConsole = False}
        $ \conn -> do
            authSucceed <- authenticate PLAIN fromGmail fromGmailPwd conn
            if authSucceed
                then do
                    let from = fromGmail ++ "@gmail.com"
                    sendPlainTextMail from from title (fromString body) conn
                    print "EMAIL SENT"
                else print "EMAIL NOT SENT"
