{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Quid2.Util.Service
  ( Config(..)
  , initCommand
  , initService
  , initServiceFull
  , fatalErr
  , UserCmd(..)
  , ServiceCmd(..)
  )
where

-- CHECK http://hackage.haskell.org/package/xdg-basedir
import           Control.Exception
import           Control.Monad
import           Data.List                      ( )

-- ,initProcess,initWebProcess
-- import Data.Char
import           Data.Maybe
import           System.Directory
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.FilePath
import           System.IO
import           System.Log.Handler.Syslog
import           System.Posix            hiding ( Start
                                                , Stop
                                                )

import           System.FileLock         hiding ( lockFile )
import           System.IO.Temp
import           System.Log.Formatter
import           System.Log.Handler             ( setFormatter )
import           System.Log.Handler.Simple
import           System.Log.Logger

-- import System.Exit
import           System.Posix.Daemonize         ( serviced
                                                , fatalError
                                                , simpleDaemon
                                                , CreateDaemon(..)
                                                )

-- import Quid2.Util.Log
-- import           Quid2.Util.Dir
data Config c =
  Config
  -- runMode::RunMode
  -- pname :: String
    { privateKeyFile :: FilePath
    , lockFile       :: FilePath
    , stateDir       :: FilePath
    , logDir         :: FilePath
    , tmpDir         :: FilePath
    , appConf        :: Maybe c
    --, logPriority    :: Priority
    --, logStdOut      :: Bool
    }
  deriving (Show)

data RunMode
  = Interactive -- Running in a GHCi session
  | Command -- Running as a compiled application from command line
  | Service -- Running as a system service
  deriving (Eq, Show)

data UserCmd cfg =
  UserCmd
    { mConfig  :: Maybe cfg
    , mService :: Maybe ServiceCmd
    }
  deriving (Eq, Show)

data ServiceCmd
  = Start
  | Restart
  | Stop
  deriving (Eq, Show)

parseUserCmd :: Read cfg => IO (UserCmd cfg)
parseUserCmd = do
  args <- getArgs
  -- dbg $ "ARGS: " ++ show args
  return $ case length args of
    0 -> UserCmd Nothing Nothing
    1 -> case head args of
      "start"   -> UserCmd Nothing (Just Start)
      "restart" -> UserCmd Nothing (Just Restart)
      "stop"    -> UserCmd Nothing (Just Stop)
      cfg       -> UserCmd (Just (read cfg)) Nothing
    _ -> error "Unexpected number of command line parameters"

initCommand
  :: Read cfg => String -> UserCmd cfg -> (Config cfg -> IO ()) -> IO ()
initCommand name userCmd realMain =
  initServiceFull name userCmd realMain Nothing

initService name realMain = do
  userCmd <- parseUserCmd
  initServiceFull name userCmd realMain Nothing

{-
A process has a unique name.
Read in its configuration from file or command line.
Creates the directory structure for logging and state persistency.
Initialise logging.
-}
initServiceFull
  :: (Read cfg)
  => String
  -> UserCmd cfg
  -> (Config cfg -> IO ())
  -> (Maybe (IO ()))
  -> IO ()
initServiceFull name userCmd realMain maybeRootMain = do
  pname <- getProgName
  when (pname /= "<interactive>" && pname /= name)
    $ error "Name does not match application name."
  let mode | pname == "<interactive>"  = Interactive
           | isJust (mService userCmd) = Service
           | otherwise                 = Command
  when (isJust maybeRootMain && mode /= Service)
       (error "Privileged actions can only be run as a service.")
  let cmd = doMain name realMain mode userCmd
  if mode /= Service
    then cmd
    else serviced $ simpleDaemon
      { program          = const cmd
      --,privilegedAction = maybe (return ()) (\f -> f cfg) maybeRootMain
      , privilegedAction = fromMaybe (return ()) maybeRootMain
      , user             = Just name
      , group            = Just name
      }

doMain :: Read a => String -> (Config a -> IO b) -> RunMode -> UserCmd a -> IO b
doMain name realMain mode userCmd = do
  userID <- getRealUserID
  home   <- fmap homeDirectory $ getUserEntryForID userID
  let lkFile     = home </> ("." ++ name ++ ".lock")
  let configFile = home </> ("." ++ name ++ ".conf")
  let appDir     = home </> '.' : name
  let stateDir   = appDir </> "state"
  let logDir     = appDir </> "log"
  --let tmpDir     = appDir </> "tmp"
  let logFile    = logDir </> "debug.txt"
  let privFile   = home </> "privateKey.quid2"
  tmpDir <- withFileLock lkFile Exclusive $ \_ -> do
    mkDir stateDir
    mkDir logDir
  --makeNewDir tmpDir
  -- makeDir tmpDir
  -- A different tmpDir per instance of application
  -- Wiped out by system
    tmpRoot <- getCanonicalTemporaryDirectory
    createTempDirectory tmpRoot name
  setupLog name mode logFile
    -- when run with sudo on osx the home dir is "/var/root"
    -- print $ "HOME: " ++ home
    -- TODO: better error reporting.
  -- conf <- if mode /= Service && length args == 1
  --                 -- read conf from command line
  --   then return . Just . read . head $ args
  mconf <- if isJust (mConfig userCmd)
    then return $ mConfig userCmd
    else do
      mconf <- try (readFile configFile)
      return $ either (\(_ :: IOException) -> Nothing) (Just . read) mconf
  when (isNothing mconf) $ dbg "No configuration provided."
    -- Any exception will cause a restart
  let cfg = Config { stateDir       = stateDir
                   , logDir         = logDir
                   , tmpDir         = tmpDir
                   , privateKeyFile = privFile
                   , lockFile       = lkFile
                   , appConf        = mconf
                   }
  handle (\(e :: SomeException) -> fatalErr (show e)) $ realMain cfg

setupLog name mode logFile = do
  updateGlobalLogger rootLoggerName
                     (setLevel $ if mode /= Service then DEBUG else INFO -- DEBUG) -- INFO
                                                                        )
    -- E.catch (removeFile logFile) (\e -> return ())
    -- On disk, register everything.
  h <- if mode == Interactive -- /= Service
    then verboseStreamHandler stderr DEBUG
    else fileHandler logFile DEBUG >>= \h -> return $ setFormatter
      h
      (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger rootLoggerName (setHandlers [h])
  -- The net logger, sends only relevant stuff.
  -- logh <- logger NOTICE
  when (mode == Service) $ do
    logh <- openlog name [PID] DAEMON WARNING
    updateGlobalLogger rootLoggerName (addHandler logh)
  warningM "Quid2.Util.Service" $ "Starting: " ++ name
    --print mode
    -- Setup log

mkDir = createDirectoryIfMissing True

fatalErr msg = do
  emergencyM "Quid2.Util.Service" msg
  fatalError msg

{-
initProcess :: String     -- ^Package name OLD:A unique name, used to identity this process, e.g.: "http"
               -> String  -- ^App name, e.g. "quid2.http" OLD:^A path identifying the particular instance being run
               -> Bool -- True if log is to be recorded on file, false otherwise
               -> IO (FilePath -- ^State directory
                     ,FilePath -- ^Log directory
                     )
initProcess packageName processName logOnFile = do

    home <- fmap homeDirectory $ getRealUserID >>= getUserEntryForID
    let appDir = home </> '.' : packageName
    let fullName   = packageName ++ "." ++ processName
    --let topDir = foldl (</>) userDir path
    let stateDir = appDir </> "state"
    let logDir   = appDir </> "log"
    let logFile  = logDir </> "debug.txt"
    mkDir stateDir
    mkDir logDir
    setupLog logOnFile

initWebProcess packageName processName address logOnFile = do
  (stateDir,logDir) <- initProcess packageName processName logOnFile

  serverLogger <- getLogger "Happstack.Server.AccessLog.Combined"
  accessFileHandler <- fileHandler (logDir </> address ++ ".access.log") DEBUG
  saveGlobalLogger $ setLevel DEBUG $ setHandlers [accessFileHandler] serverLogger

  return (stateDir,logDir)
-}
dbg = debugM "Quid2.Util.Service"

info = infoM "Quid2.Util.Service"

errM = errorM "Quid2.Util.Service"
