{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use replicateM" #-}

module Test(run,Test,wwwTest,wwwTest_,notContains,ovhTest) where

import           Control.Concurrent.Async    (async, waitCatch)
import           Control.Monad
import           Control.Retry               (recoverAll, retryPolicy,
                                              retryPolicyDefault)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           Data.String                 (IsString (fromString))
import           GHC.Arr                     (badSafeIndex)
import           Network.HostName
import           Network.HTTP.Client.Conduit (Response (responseStatus))
import           Network.Pushover
import           Network.Pushover.Request    (Request (priority))
import           Network.Top                 (async, seconds, threadDelay)
import           Repo.Memory                 ()
import           System.Environment          (getArgs)
import           System.Exit                 (ExitCode)
import           System.Timeout              (timeout)
import           Test.GPG                    (gpgDecryptValue)
import           Test.OVH
import           Test.Types                  (Test (check, name, source, timeoutInSecs))
import           Test.WWW                    (notContains, wwwTest, wwwTest_)
import           Util

data PushoverId = PushoverId {user,api::String} deriving (Show,Read)

t = run $ [wwwTest ("https://quid2.org","Flat")]

-- NOTE: Need to `make mov` to transfer decoding key
run :: [Test] -> IO ()
run tests = do
  po <- gpgDecryptValue "PushoverId.gpg"
  -- notify po Lowest $ concat ["test@",name," started"]
  testLoop po tests

testLoop :: PushoverId -> [Test] -> IO ()
testLoop po tests = do
   -- print po

   -- forever $ runAll
  mapM_ runAll [0..]
      where
        hour = 60
        runAll i = do
            when (i `mod` (12*hour) == 0) $ notify po Lowest "running"
            failedTests <- filter (isJust . snd) <$> runTests tests
            unless (null failedTests) $ void $ notify po High (show  . map (\(name,Just err) -> unwords [name,err]) $ failedTests)
            threadDelay (seconds 60)

-- Android notifications via https://pushover.net/
notify :: PushoverId -> Priority -> String -> IO ()
notify po pri msg = do
    let Right userKey = makeToken $ fromString $ user po
    let Right apiKey  = makeToken $ fromString $ api po

    hname <- getHostName
    let me = "test@" ++ hname

    -- print $ unlines ["Notifying: ",msg]
    let msg' = text . fromString . take 256 $ concat [me,": ",msg]
    -- r <- recoverAll retryPolicyDefault $ \_ -> sendMessage apiKey userKey msg'

    -- PROB: Emergency level requires Retry/Expire parameters (see https://pushover.net/api#messages)
    -- that do not seem to be supported by https://hackage.haskell.org/package/pushover
    r <- recoverAll retryPolicyDefault $ \_ -> sendRequest $ (createRequest apiKey userKey msg') {priority=Just pri}

    -- FAIL on pushover failure
    case status r of
      Success   -> return ()
      Failure _ -> print $ "EXITING: pushover failure: " ++ show r

runTests :: Traversable t => t Test -> IO (t (String, Maybe String))
runTests = mapM runTest

{-
>>> sequence [return $ Just "ok",return $ Just "badSafeIndex"]
Just ["ok","badSafeIndex"]

>>> sequence [Just "ok",Just "badSafeIndex",Nothing]
Nothing
-}
-- Returns (test name ,Nothing if ok or Just error)
runTest :: Test -> IO (String, Maybe String)
-- runTest t = do
--   rs <- sequence <$> replicateM 3 (runTest_ t)
--   return (name t, head <$> rs)

-- r t = (name t,) <$> go 2
runTest t = (name t,) <$> go 2
 where
   go 0 = runTest_ t
   go n  = do
    rt <- runTest_ t
    case rt of
      Nothing -> return Nothing
      Just _  -> threadDelay (secs 30) >> go (n-1)

runTest_ :: Test -> IO (Maybe String)
runTest_ t = do
    r <- chk <$> (async (timeout (seconds $ timeoutInSecs t) (source t)) >>= waitCatch)
    -- print r
    return $ case r of
        Left e  -> Just e
        Right c -> check t c
  where

    -- chk :: Either SomeException (Maybe B8.ByteString) -> Either B8.ByteString B8.ByteString
    chk (Left exp)       = Left (fromString . show $ exp)
    chk (Right Nothing)  = Left "Test Timeout"
    chk (Right (Just s)) = Right s


-- import System.Directory
-- import qualified Control.Exception as E

-- gpgDecrypt :: FilePath -> IO String
-- gpgDecrypt f = do
--         gpgbin <- getGpgBin
--         ifM (doesFileExist f)
--                 ( writeReadProcessEnv gpgbin ["--decrypt", f] Nothing Nothing Nothing
--                 , return ""
--                 )

-- writeReadProcessEnv
--         :: FilePath
--         -> [String]
--         -> Maybe [(String, String)]
--         -> (Maybe (Handle -> IO ()))
--         -> (Maybe (Handle -> IO ()))
--         -> IO String
-- writeReadProcessEnv cmd args environ writestdin adjusthandle = do
--         (Just inh, Just outh, _, pid) <- createProcess p

--         maybe (return ()) (\a -> a inh) adjusthandle
--         maybe (return ()) (\a -> a outh) adjusthandle

--         -- fork off a thread to start consuming the output
--         output  <- hGetContents outh
--         outMVar <- newEmptyMVar
--         _ <- forkIO $ E.evaluate (length output) >> putMVar outMVar ()

--         -- now write and flush any input
--         maybe (return ()) (\a -> a inh >> hFlush inh) writestdin
--         hClose inh -- done with stdin

--         -- wait on the output
--         takeMVar outMVar
--         hClose outh

--         -- wait on the process
--         forceSuccessProcess p pid

--         return output

--   where
--         p = (proc cmd args)
--                 { std_in = CreatePipe
--                 , std_out = CreatePipe
--                 , std_err = Inherit
--                 , env = environ
--                 }

