{-# LANGUAGE NoMonomorphismRestriction ,ScopedTypeVariables #-}
module Quid2.Util.Service(Config(..),initService,initServiceFull,fatalErr) where

-- ,initProcess,initWebProcess

import Data.Char
import Data.Maybe
import Data.List()
import Control.Monad
import System.FilePath
import System.Directory
import System.IO
import System.Log.Handler.Syslog
import System.Posix
import Control.Exception
import System.Environment(getArgs,getProgName)
import System.Exit
import System.Posix.Daemonize
import System.Log.Formatter
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler(setFormatter)
-- import Quid2.Util.Log
import Quid2.Util.Dir

data Config c = Config {
  -- runMode::RunMode
  -- pname :: String
  privateKeyFile :: FilePath
  ,stateDir :: FilePath
  ,logDir ::  FilePath
  ,tmpDir ::  FilePath
  ,appConf :: Maybe c
  } deriving (Show)

data RunMode = Interactive -- Running in a GHCi session
             | Command     -- Running as a compiled application from command line
             | Service     -- Running as a system service
  deriving (Eq,Show)

initService name realMain = initServiceFull name realMain Nothing

{-
A process has a unique name.
Read in its configuration from file or command line.
Creates the directory structure for logging and state persistency.
Initialise logging. 
-}
initServiceFull :: (Read cfg,Show cfg) => String -> (Config cfg -> IO ()) -> (Maybe (IO ())) -> IO ()
initServiceFull name realMain maybeRootMain = do
    pname <- getProgName
    when (pname /= "<interactive>" && pname /= name) $ error "Name does not match application name."
    args <- getArgs
    -- dbg $ "ARGS: " ++ show args 
    let mode | pname == "<interactive>" = Interactive
             | length args == 1 && head args `elem` ["start", "restart", "stop"] = Service
             | otherwise = Command

    --(cfg,cmd) <- initService2 mode args name realMain
    --info $ "initService2: " ++ show mode ++ " " ++ show cfg
    -- setupLog name mode logFile
    -- when (isNothing conf) $ dbg "No configuration provided."
    when (isJust maybeRootMain && mode /= Service) (error "Privileged actions can only be run as a service.")

    let cmd = doMain name realMain mode args
    if mode /= Service
      then cmd
      else serviced $ simpleDaemon {
      program = const $ cmd
      --,privilegedAction = maybe (return ()) (\f -> f cfg) maybeRootMain
      ,privilegedAction = fromMaybe (return ()) maybeRootMain
      ,user  = Just name
      ,group = Just name
      }

doMain name realMain mode args = do
    userID <- getRealUserID
    home <- fmap homeDirectory $ getUserEntryForID userID
    let configFile = home </> ("." ++ name ++ ".conf")
    let appDir = home </> '.' : name
    let stateDir = appDir </> "state"
    let logDir   = appDir </> "log"
    let tmpDir   = appDir </> "tmp"
    let logFile  = logDir </> "debug.txt"
    let privFile = home </> "privateKey.quid2"
    mkDir stateDir
    mkDir logDir
    makeNewDir tmpDir
    setupLog name mode logFile
    -- when run with sudo on osx the home dir is "/var/root"
    -- print $ "HOME: " ++ home

    -- TODO: better error reporting.
    conf <- if mode /= Service && length args == 1
            -- read conf from command line
            then return . Just . read . head $ args
            else do
              mconf <- try (readFile configFile)
              return $ either (\(e::IOException) -> Nothing) (Just . read) mconf
    when (isNothing conf) $ dbg "No configuration provided."

    -- Any exception will cause a restart
    let cfg = Config {stateDir=stateDir,logDir=logDir,tmpDir=tmpDir,privateKeyFile=privFile,appConf=conf}

    handle (\(e::SomeException) -> fatalErr (show e)) $ realMain cfg

setupLog name mode logFile = do
    --print mode
    -- Setup log
    updateGlobalLogger rootLoggerName  (setLevel $ if mode /= Service then DEBUG else INFO) -- DEBUG) -- INFO

    -- E.catch (removeFile logFile) (\e -> return ())
    -- On disk, register everything. 
    h <- if mode == Interactive -- /= Service
         then verboseStreamHandler stderr DEBUG
         else fileHandler logFile DEBUG >>= \h -> return $ setFormatter h (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger rootLoggerName (setHandlers [h])

    -- The net logger, sends only relevant stuff.
    -- logh <- logger NOTICE
    logh <- openlog name [PID] DAEMON WARNING
    updateGlobalLogger rootLoggerName (addHandler logh)

    warningM "Quid2.Util.Service" $ "Starting: " ++ name

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

