module App.Types where

data Config c = Config
    -- runMode::RunMode
    -- pname :: String
    { --privateKeyFile :: FilePath
      lockFile :: FilePath
    , stateDir :: FilePath
    , logDir :: FilePath
    , tmpDir :: FilePath
    , appConf :: Maybe c
    --, logPriority    :: Priority
    --, logStdOut      :: Bool
    }
    deriving (Show)
