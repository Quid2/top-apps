{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Test(run,Test,wwwTest,wwwTest_,notContains,ovhTest) where

import           Control.Concurrent.Async    (async, waitCatch)
import           Control.Monad               (forever, unless, void)
import           Control.Retry               (recoverAll, retryPolicyDefault)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust)
import           Data.String                 (IsString (fromString))
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
import Network.HostName
import Control.Monad

data PushoverId = PushoverId {user,api::String} deriving (Show,Read)

t = run $ map wwwTest [("https://quid2.org","Flat")]

run :: [Test] -> IO ()
run tests = do
  po <- gpgDecryptValue "PushoverId.gpg"
  -- notify po Lowest $ concat ["test@",name," started"]
  testLoop po tests

testLoop :: PushoverId -> [Test] -> IO ()
testLoop po tests = do
   -- print po 
  hname <- getHostName

   -- forever $ runAll
  mapM_ (runAll hname) [0..] 
      where
        hour = 60
        runAll hname i = do
            when (i `mod` (12*hour) == 0) $ notify po Lowest $ concat ["test@",hname," running"]
            failedTests <- filter (isJust . snd) <$> runTests tests
            unless (null failedTests) $ void $ notify po Emergency (show . map (\(name,Just err) -> unwords [name,err]) $ failedTests)
            threadDelay (seconds 60)

-- Android notifications via https://pushover.net/
notify :: PushoverId -> Priority -> String -> IO ()
notify po pri msg = do
    let Right userKey = makeToken $ fromString $ user po
    let Right apiKey  = makeToken $ fromString $ api po

    print $ unlines ["Notifying: ",msg]
    let msg' = text . fromString . take 256 $ msg
    -- r <- recoverAll retryPolicyDefault $ \_ -> sendMessage apiKey userKey msg'
    r <- recoverAll retryPolicyDefault $ \_ -> sendRequest $ (createRequest apiKey userKey msg') {priority=Just pri}

    -- FAIL on pushover failure
    case status r of
      Success   -> return ()
      Failure _ -> print $ "EXITING: pushover failure: " ++ show r

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

