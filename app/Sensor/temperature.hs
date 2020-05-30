{-# LANGUAGE ScopedTypeVariables       #-}

-- |Periodically send reading of local temperature
-- BUG: no error logged if temperature reading fails
module Main where

import           Data.Maybe
import           Data.Time.Util
import           Network.HostName
import           Network.Top             hiding ( Config )
import           Sensor.Model
import           System.Exit
import           System.Process
import           Text.Regex.TDFA
import           Sensor
import           App

main = app "top-temperature" $ sensor localTemperature (seconds 10)

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

