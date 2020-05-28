{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |Periodically send current time and local temperature
-- BUG: no error logged if temperature reading fails
module Main where

import           Control.Concurrent
import           Data.Bifunctor
import           Data.Either
import           Data.Maybe
import           Data.Time.Util
import           Network.HostName
import           Network.Top             hiding ( Config )
import           Quid2.Util.Service
import           Sensor.Model
import           System.Exit
import           System.IO                      ( stdout )
import           System.Process
import           Text.Regex.TDFA

r = run $ recordType (Proxy :: Proxy (SensorReading Celsius String))

t = sensor currentTime (milliseconds 500)

data MyConfig =
  MyConfig
    { debugLevel :: Priority
    }
  deriving (Show, Read)

serviceName = "top-sensors"

main = initService serviceName setup

setup :: Config MyConfig -> IO ()
setup cfg = do
  let appCnf   = appConf cfg
  let dbgLevel = fromMaybe WARNING (debugLevel <$> appCnf)
  logLevelOut dbgLevel stdout
  dbgS $ show cfg
  -- sensor currentTime (milliseconds 500)
  forkIO $ sensor currentTime (milliseconds 500)
  sensor localTemperature (seconds 10)

p = parseTemperature "More\nCore 0:       +44.7xC  ...dsds"

localTemperature = do
  dbgS "localTemperature"
  place <- getHostName
  dbgShow place
  temp <- cpuTemperature
  dbgShow temp
  return $ SensorReading temp place

-- |Returns CPU temperature (in Linux systems with working 'sensors')
cpuTemperature :: IO Celsius
cpuTemperature = do
  (ExitSuccess, out, err) <- readProcessWithExitCode "sensors" [] ""
  dbgShow out
  dbgShow err
  return . Celsius . fromJust . parseTemperature $ out
  -- return $ Celsius 33.3

parseTemperature :: String -> Maybe Float
parseTemperature str =
  let (_, _, _, matches) :: (String, String, String, [String]) =
          str =~ "Core 0:[ ]+([+-][1-9][0-9]*[.][0-9]+).C"
  in  if length matches == 1
        then
          let n = head matches
          in  Just (read (if head n == '+' then tail n else n) :: Float)
        else Nothing

sensor0 read minInterval = do
  r <- read
  dbgShow r


-- | Read a sensor and send over value on corresponding channel
sensor
  :: forall a
   . (NFData a, Flat a, Model a, Show a, Eq a)
  => IO a
  -> Int
  -> IO ()
sensor read minInterval = runA $ \conn -> do
  dbgS "started sensor"
  let io = first show <$> strictTry read
  let out v = when (isRight v) $ output conn (let Right o = v in o)
  let loop v = do
        threadDelay minInterval
        v1 <- io
        --dbgShow v1
        when (v1 /= v) $ out v1
        loop v1
  v <- io
  dbgShow v
  out v
  loop v

runA = runAppForever def ByType
-- run app = runApp def ByType app
-- dbgShow = dbgS . show
-- dbgSh = dbgS . show
-- dbgShow _ = return ()
