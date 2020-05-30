module App
  ( app
  )
where

import           Network.Top             hiding ( Config )
import           Quid2.Util.Service
import           Data.Maybe
import           System.IO                      ( stdout )


data MyConfig =
  MyConfig
    { debugLevel :: Priority
    }
  deriving (Show, Read)

app serviceName serviceIO = initService serviceName $ \cfg -> do
  setup cfg
  --run $ recordType (Proxy :: Proxy (SensorReading Celsius String))
  serviceIO

setup :: Config MyConfig -> IO ()
setup cfg = do
  let appCnf   = appConf cfg
  let dbgLevel = fromMaybe WARNING (debugLevel <$> appCnf)
  logLevelOut dbgLevel stdout
  dbgS $ show cfg

