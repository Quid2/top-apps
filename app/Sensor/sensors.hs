{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import           Control.Concurrent
import           Data.Either
import           Data.Either.Extra
import           Data.Maybe
import           Data.Time.Util
import           Network.Top
import           System.Exit
import           System.IO
import           System.Process
import           Text.Regex.TDFA

main = do
  sensor currentTime (milliseconds 500)
  sensor cpuTemperature (seconds 10)

-- t ::
t = parseTemperature "More\nCPU Temp:    +33.0°C ...dsds"
-- t = "CPU Temp:    +33.0°C  (high = +80.0°C, hyst = +75.0°C)  sensor = diode" =~ "CPU Temp:[:blank:]+([+-][1..9][0..9]*[.][0..9]+)(.+)"

-- Returns on CPU temperature (in Linux systems with 'sensors')
cpuTemperature = do
  (ExitSuccess,out,err) <- readProcessWithExitCode "sensors" [] ""
  return . fromJust . parseTemperature $ out

-- TODO: return as Float
parseTemperature :: String -> Maybe Int
parseTemperature str = let (_,_,_,matches) :: (String,String,String,[String]) = str =~ "CPU Temp:[ ]+([+-][1-9][0-9]*[.][0-9]+)°C"
            in if length matches == 1
               then let n = head matches
                    in Just . round $ (read (if head n == '+' then tail n else n) :: Float)
               else Nothing

sensor0 read minInterval = do
  r <- read
  print r

sensor read minInterval = run $ \conn -> do
  let io = either (Left . show) Right <$> strictTry read
  let out v = when (isRight v) $ output conn (fromRight v)
  let loop v = do
            threadDelay minInterval
            v1 <- io
            when (v1 /= v) $ out v1
            loop v1


  v <- io
  out v
  loop v

run app = forkIO $ do
  threadDelay (seconds 30)
  runClientForever def ByType app

