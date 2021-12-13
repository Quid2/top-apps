{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Test(run,Test,wwwTest) where

import           Control.Concurrent.Async (async, waitCatch)
import           Control.Monad            (forever, unless, void)
import qualified Data.ByteString.Char8    as B8
import qualified Data.Map                 as M
import           Data.Maybe               (isJust)
import           Data.String              (IsString (fromString))
import           Network.Pushover         (makeToken, sendMessage, text)
import           Network.Top              (async, seconds, threadDelay)
import           Repo.Memory              ()
import           System.Environment       (getArgs)
import           System.Timeout           (timeout)
import           Test.Types               (Test (check, name, source, timeoutInSecs))
import           Test.WWW                 (wwwTest)

run :: [Test] -> IO ()
run tests = do
  [pushoverUserKey,pushoverApiKey] <- getArgs
  -- print [pushoverUserKey,pushoverApiKey]
  testLoop pushoverUserKey pushoverApiKey tests

testLoop :: [Char] -> [Char] -> [Test] -> IO b
testLoop pushoverUserKey pushoverApiKey tests = do

   forever $ runAll >> threadDelay (seconds 60)
      where
        runAll = do
            failedTests <- filter (isJust . snd) <$> runTests tests
            unless (null failedTests) $ void $ notify pushoverUserKey pushoverApiKey (show . map (\(name,Just err) -> unwords [name,err]) $ failedTests)

-- Android notifications via https://pushover.net/
notify :: [Char] -> [Char] -> [Char] -> IO ()
notify pushoverUserKey pushoverApiKey msg = do
    let Right userKey = makeToken $ fromString pushoverUserKey
    let Right apiKey  = makeToken $ fromString pushoverApiKey

    print msg
    r <- sendMessage apiKey userKey (text . fromString . take 256 $ msg)
    -- TODO: detect pushover failure
    print r

runTests :: Traversable t => t Test -> IO (t (String, Maybe String))
runTests = mapM runTest

-- Returns (test name ,Nothing if ok or Just error)
runTest :: Test -> IO (String, Maybe String)
runTest t = do
    r <- chk <$> (async (timeout (seconds $ timeoutInSecs t) (source t)) >>= waitCatch)
    -- print r
    return $ (name t,) $ case r of
        Left e  -> Just e
        Right c -> check t c
  where
    -- chk :: Either SomeException (Maybe B8.ByteString) -> Either B8.ByteString B8.ByteString
    chk (Left exp)       = Left (fromString . show $ exp)
    chk (Right Nothing)  = Left "Test Timeout"
    chk (Right (Just s)) = Right s

