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

t = recordType def (Proxy::Proxy Int)

main = do
  forkIO $ sensor currentTime (milliseconds 500)
  sensor cpuTemperature (seconds 10)

-- t ::
p = parseTemperature "More\nCore 0:       +44.7 C  ...dsds"

-- Returns CPU temperature (in Linux systems with working 'sensors')
cpuTemperature :: IO Int
cpuTemperature = do
  (ExitSuccess,out,err) <- readProcessWithExitCode "sensors" [] ""
  return . fromJust . parseTemperature $ out

-- TODO: return as Float
parseTemperature :: String -> Maybe Int
parseTemperature str = let (_,_,_,matches) :: (String,String,String,[String]) = str =~ "Core 0:[ ]+([+-][1-9][0-9]*[.][0-9]+) C"
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

run app = do
  -- threadDelay (seconds 30)
  runClientForever def ByType app

