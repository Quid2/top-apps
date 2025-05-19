{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
TODO:
Add memory/elapsed time
Add privileged support for WWW
-}
module App
  ( Config (..),
    Mode (..),
    app,
    AppCode (..),
    AppLog,
    infoApp,
    warnApp,
    errApp,
    netLog,
    logTests,
  )
where

-- import qualified Data.Text as T
-- import Data.Time.Clock.POSIX (getPOSIXTime)
-- import RIO
-- import Stats.App (registerAppMetrics, registerAppNames)
-- import Stats.Host (registerHostName)
-- import System.Metrics (Store, createLabel, newStore)
-- import qualified System.Metrics.Counter as Counter
-- import qualified System.Metrics.Distribution as Distribution
-- import qualified System.Metrics.Label as Label
-- import System.Remote.Monitoring.Top (
--     TopOptions (debug, flushInterval),
--     def,
--     forkEkgTop,
--  )

import Control.Concurrent (forkIO)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text, pack)
import NetLog
import Network.HostName (getHostName)
import Network.Top hiding (Config, Logger, err, info, warn)
import System.Directory
import System.Environment (getArgs, getProgName)
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix hiding
  ( Start,
    Stop,
  )
import Test (runTests)
import Text.Read (readMaybe)
import Turtle hiding (err)
import Util (periodically)
import Prelude

app :: (Read cfg) => AppCode cfg -> IO ()
app appCode = do
  appNameS <- getProgName
  let appName = pack appNameS
  appDir <- getCurrentDirectory
  let stateDir = appDir </> "state"
  let logDir = appDir </> "log"
  -- let logFile    = logDir </> "debug.txt"
  mkDir stateDir
  mkDir logDir
  tmpRoot <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory tmpRoot appNameS
  host <- pack <$> getHostName
  let cfg =
        Config
          { name = appName,
            key = AppID {appID = appName, hostID = host, instanceID = ""},
            mode = RunMode,
            stateDir = stateDir,
            logDir = logDir,
            tmpDir = tmpDir,
            appConf = Nothing
          }

  cfg' <- parseUserCmd cfg
  -- print cfg'

  when (hasState appCode) $ periodically 60 (pushState cfg)

  -- Run/Test till interrupted
  case mode cfg' of
    RunMode -> do
      forkIO $ appRun appCode cfg'
      basicRun cfg'
    TestMode -> do
      forkIO $ appTest appCode cfg'
      basicTest cfg'

mkDir :: FilePath -> IO ()
mkDir = createDirectoryIfMissing True

-- >>> pushState
pushState cfg = do
  rt <- shell "git add state;git commit -m \"save state\";git push" empty
  case rt of
    ExitFailure e -> return () -- FIX errApp netLog (key cfg) (pack $ "Save state failed: " <> show e)
    _ -> return ()

parseUserCmd :: (Read c) => Config c -> IO (Config c)
parseUserCmd cfg0 = do
  args <- getArgs
  let hasKey = not (null args) && isJust (readMaybe (head args) :: Maybe Integer)
  let (maybeKey :: Maybe Integer, rargs) = if hasKey then (readMaybe (head args), tail args) else (Nothing, args)
  let cfg = cfg0 {key = (key cfg0) {instanceID = maybe "" (pack . show) maybeKey}}
  return $ case rargs of
    [] -> cfg
    ["run"] -> cfg
    ["test"] -> cfg {mode = TestMode}
    ["run", appCfg] -> cfg {appConf = Just $ read appCfg}
    ["test", appCfg] -> cfg {mode = TestMode, appConf = Just $ read appCfg}
    _ -> error $ "Unexpected command line parameters: " ++ show args

basicRun :: Config c -> IO ()
basicRun cfg = forever $ do
  -- send heartbeat
  info netLog (key cfg) ("OK" :: Text)
  threadDelay (seconds 1)

basicTest :: Config c -> IO ()
basicTest cfg = run loop
  where
    loop conn = do
      mr :: Maybe AppLog <- inputWithTimeout 3 conn
      case mr of
        Nothing -> errApp netLog (key cfg) ("No heartbeat detected" :: Text) >> loop conn
        Just _ -> loop conn

-- >>> once
-- Right [(),(),(),(),(),(),(),(),()]
once :: IO (Either String [()])
once = do
  run $ recordType (Proxy :: Proxy AppLog)

type AppLog = Log AppID Text Text Text

infoApp, warnApp, errApp :: Logger AppID Text Text Text -> AppID -> Text -> IO ()
infoApp = info
warnApp = warn
errApp = err

logTests cfg tests = periodically 60 $ runTests tests >>= mapM_ (errApp netLog (key cfg) . pack)

data Config c = Config
  { name :: Text,
    key :: AppID,
    mode :: Mode,
    stateDir :: FilePath,
    logDir :: FilePath,
    tmpDir :: FilePath,
    appConf :: Maybe c
  }
  deriving (Show)

data Mode = RunMode | TestMode deriving (Eq, Show)

data AppCode cfg = AppCode
  { appRun, appTest :: Config cfg -> IO (),
    hasState :: Bool
  }

-- Every app instance is uniquely identified (multiple app instances can run on the same host)
data AppID = AppID {appID :: Text, hostID :: Text, instanceID :: Text} deriving (Eq, Ord, Show, Generic, Flat)

instance Model AppID

-- app :: (Store -> IO b) -> IO b
-- app op = do
--     store <- newStore
--     registerAppMetrics store
--     registerAppNames store
--     registerHostName store
--     forkEkgTop def{flushInterval = 60, debug = False} store
--     op store

-- host :: IO ()
-- host = app $ \store -> do
--     registerHostMetrics store

-- t :: IO ()
-- t = app $ \store -> do
--     requests <- createCounter "myapp.request_count" store
--     -- Every time we receive a request:
--     Counter.inc requests
--     counter <- createCounter "iterations" store
--     label <- createLabel "args" store
--     Label.set label "some text string"
--     event <- createDistribution "runtime" store

--     let loop n = do
--             t <- timed $ evaluate $ mean [1 .. n]
--             Distribution.add event t
--             threadDelay 2000
--             Counter.inc counter
--             loop n
--     loop 1000000

-- timed :: IO a -> IO Double
-- timed m = do
--     start <- getTime
--     m
--     end <- getTime
--     return $! end - start

-- getTime :: IO Double
-- getTime = realToFrac `fmap` getPOSIXTime
