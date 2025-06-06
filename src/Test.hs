{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test
  ( runTests,
    wwwTest,
    module Test.Types,
  )
where

import Control.Concurrent.Async (async, waitCatch)
import Control.Retry
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.String (IsString (fromString))
import GHC.Arr (badSafeIndex)
import Network.HostName
import Network.Top (async, seconds, threadDelay)
import Repo.Memory ()
import System.Environment (getArgs)
import System.Exit (ExitCode)
import Test.OVH
import Test.Types
import Test.WWW (notContains, wwwTest, wwwTest_)
import Util

-- main = run [ovhTest "KS-17"] --
-- [ovhTest "KS-1",ovhTest "KS-2"] ++

{-
Run a list of tests concurrently and return list of failures when all are done or timed out

>>> runTests [wwwTest ("http://net.quid2.org/","top-router"),wwwTest ("http://net.quid2.org/","top-sdsdroutery"),wwwTest ("http://net.quid2.org/","top-router")]
["http://net.quid2.org/: does not contain \"top-sdsdroutery\""]
-}
runTests :: [Test] -> IO [String]
runTests tests = do
  threads <- mapM (async . runTest) tests
  catMaybes <$> mapM (fmap chk . waitCatch) threads
  where
    chk (Left exp) = Just (fromString . show $ exp)
    chk (Right Nothing) = Nothing
    chk (Right (Just s)) = Just s

-- >>> runTest $ wwwTest ("http://net.quid2.org/","top-router")
-- Nothing
-- >>> runTest $ wwwTest ("http://net.quid2.org/","top-routery")
-- Just "http://net.quid2.org/: does not contain \"top-routery\""
-- runTest t = do
--   r <- timeout (seconds $ timeoutInSecs t) (apply t)
--   return $ fmap (\r -> name t <> ": " <> r) $ case r of
--     Nothing -> Just "Timeout"
--     Just c -> check t c
--   where
--     -- chk :: Either SomeException (Maybe B8.ByteString) -> Either B8.ByteString B8.ByteString
--     chk (Left exp) = Left (fromString . show $ exp)
--     chk (Right Nothing) = Left "Test Timeout"
--     chk (Right (Just s)) = Right s

-- t = run logger [wwwTest ("https://quid2.org", "Flat")]

-- NOTE: Need to `make mov` to transfer decoding key
-- run :: Logger ->  [Test] -> IO ()
-- run logger tests = do
--   let name = "here"
--   testLoop logger tests

-- testLoop :: PushoverId -> [Test] -> IO ()
-- testLoop logger tests = do
--   mapM_ runAll [0 ..]
--   where
--     hour = 60
--     runAll i = do
--       when (i `mod` (12 * hour) == 0) $ info logger "running"
--       failedTests <- filter (isJust . snd) <$> runTests tests
--       unless (null failedTests) $ void $ err logger (show . map (\(name, Just err) -> unwords [name, err]) $ failedTests)
--       threadDelay (seconds 60)
