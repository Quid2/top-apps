{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Check that the key top services are working correctly.
-- | In case of failure send error to designated email.
-- | Start as:
-- | top-test <gmail account> <gmail account password>
-- | Example:
-- | top-test johnford 3as42
module Main where

import           Control.Concurrent.Async
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.String
import           Data.Time.Util
import           Network.HaskellNet.SMTP.SSL
import           Network.Top
import           Repo.Memory
import           System.Environment
import           System.Timeout

t = main

main :: IO ()
main = do
  [gmail, gmailPwd] <- getArgs
  forever $ do
    failedTests <- filter (isJust . snd) <$> runTests [testRepo, testSensors]
    unless (null failedTests) $
      email "TOP FAILURE" (show failedTests) gmail gmailPwd
    threadDelay (seconds 60)

testSensors :: Test
testSensors =
  Test "Sensors" 10 $ do
    mt :: Time <- runApp def ByType input
    return True

testRepo :: Test
testRepo =
  Test "RepoDB" 60 $ do
    let [(boolRef, boolADT)] = M.toList $ absEnv (Proxy :: Proxy Bool)
    r <- ((== boolADT) <$>) <$> run (solveAbsRef boolRef)
    return $ r == Right True

data Test =
  Test
    { name          :: String
    , timeoutInSecs :: Int
    , op            :: IO Bool
    }

runTests = mapM runTest

runTest :: Test -> IO (String, Maybe String)
runTest t =
  async (timeout (seconds $ timeoutInSecs t) (op t)) >>=
  ((name t, ) . chk <$>) . waitCatch

chk (Right Nothing)      = Just "Test timeout"
chk (Right (Just False)) = Just "Wrong Test Result"
chk (Right (Just True))  = Nothing
chk (Left exp)           = Just (show exp)

email title body fromGmail fromGmailPwd = do
  let from = fromGmail ++ "@gmail.com"
  print $ unwords ["EMAIL", title, body]
  doSMTPSTARTTLS "smtp.gmail.com" $ \conn -> do
    authSucceed <- authenticate PLAIN fromGmail fromGmailPwd conn
    if authSucceed
      then do
        sendPlainTextMail from from title (fromString body) conn
        print "EMAIL SENT"
      else print "EMAIL NOT SENT"
      -- email title body fromGmail fromGmailPwd = do
--   let from = fromGmail ++ "@gmail.com"
--   sendGmail
--     (fromString from)
--     (fromString fromGmailPwd)
--     (Address Nothing (fromString from))
--     [Address Nothing (fromString from)]
--     []
--     []
--     (fromString title)
--     (fromString body)
--     []
--     (seconds 10)
-- NO SSL support, won't work with gmail
-- import           Network.Mail.SMTP
-- email title body fromGmail fromGmailPwd = do
--   print $ unwords ["EMAIL", title, body]
--   let address = (Address Nothing $ fromString $ fromGmail ++ "@gmail.com")
--    in sendMail "smtp.gmail.com" $
--       simpleMail
--         address
--         [address]
--         []
--         []
--         (fromString title)
--         [plainTextPart $ fromString body]
