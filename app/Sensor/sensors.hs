{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Control.Concurrent
import           Data.Either
import           Data.Either.Extra
import           Data.Maybe
import           Data.Time.Util
import           Network.HostName
import           Network.Top hiding (dbg)
import           Sensor.Model
import           System.Exit
--import           System.IO
import           System.Process
import           Text.Regex.TDFA

r = do
  recordType def (Proxy::Proxy (SensorReading Celsius String))

t = sensor currentTime (milliseconds 500)

main = do
  forkIO $ sensor currentTime (milliseconds 500)
  sensor localTemperature (seconds 10)

p = parseTemperature "More\nCore 0:       +44.7xC  ...dsds"

localTemperature = do
  dbg "localTemperature"
  place <- getHostName
  dbg place
  temp <- cpuTemperature
  dbg temp
  return $ SensorReading temp place

-- Returns CPU temperature (in Linux systems with working 'sensors')
cpuTemperature :: IO Celsius
cpuTemperature = do
  (ExitSuccess,out,err) <- readProcessWithExitCode "sensors" [] ""
  dbg out
  dbg err
  return . Celsius . fromJust . parseTemperature $ out
  -- return $ Celsius 33.3

parseTemperature :: String -> Maybe Float
parseTemperature str = let (_,_,_,matches) :: (String,String,String,[String]) = str =~ "Core 0:[ ]+([+-][1-9][0-9]*[.][0-9]+).C"
            in if length matches == 1
               then let n = head matches
                    in Just (read (if head n == '+' then tail n else n) :: Float)
               else Nothing

sensor0 read minInterval = do
  r <- read
  dbg r

sensor
  :: (NFData a, Flat a, Model a, Show a, Eq a) =>
     IO a -> Int -> IO ()
sensor read minInterval = run $ \conn -> do
  let io = either (Left . show) Right <$> strictTry read
  let out v = when (isRight v) $ output conn (fromRight v)
  let loop v = do
            threadDelay minInterval
            v1 <- io
            dbg v1
            when (v1 /= v) $ out v1
            loop v1

  v <- io
  dbg v
  out v
  loop v

run app = do
  -- threadDelay (seconds 30)
  runAppForever def ByType app

-- dbg = print
dbg _ = return ()
  
