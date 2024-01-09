{-# LANGUAGE PackageImports #-}
module Quid2.Util.Time(now
                      ,HMS(..),hms,timeDateTime
                      ,wait,waitFor,timeout,timeOut
                      ,msecs,secs,minutes,timeF,parseDDMMMYY,parseDDMMYY,parseAmericanDate,formatAsAmericanDate
                      ) where

import           Prelude
-- import Prelude(Show)
import           Control.Concurrent     (threadDelay)
import           Data.List
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           System.Time
import           System.Timeout
-- import System.Locale
import           Control.Applicative
import           Control.DeepSeq        (NFData, ($!!))
import           Control.Exception
import           Control.Monad.IO.Class
import           "time" Data.Time.Calendar     (toGregorian)
-- import "time" Data.Time.Format

t = timeDateTime

waitFor :: MonadIO m => Int -> m ()
waitFor = liftIO . wait

msecs t = t * 1000

secs t = t * msecs 1000

minutes t = t * secs 60

wait = threadDelay

-- Current time in seconds (unix epoch).
now :: IO Integer
now = do
  TOD now _  <- getClockTime
  return now

x :: IO ()
x = timeOut (secs 1) $ wait (secs 15)

-- stupid
timeOut :: NFData b => Int -> IO b -> IO b
timeOut microSecs op = ((fromMaybe (error "Timeout")) <$> timeout microSecs op) >>= (evaluate $!!)

-- Hour Minutes Seconds
data HMS = HMS {hh,mm,ss::Int} deriving (Eq,Ord)

instance Show HMS where show hms = concat . intersperse ":" . map show $ [hh hms,mm hms,ss hms]

showHHMMSS (h,m,s) = concat . intersperse ":" . map show $ [h,m,s]

-- UTC/GMT time
hms = do
  numSecs <- fmap ((`div` 1000000000000) . fromEnum . utctDayTime) getCurrentTime
  let secs = (`mod` 60) numSecs
  let numMinutes = ((`mod` 60) . (`div` 60)) numSecs
  let numHours = (`div` 60) . (`div` 60) $ numSecs
  return $ HMS numHours numMinutes secs

timeMM = timeF "%M"
timeHHMM = timeF "%H:%M"
timeHHMMSS = timeF "%H:%M.%S"
timeDateTime = timeF "%F %H:%M.%S"
timeF format = fmap (formatTime defaultTimeLocale format) getCurrentTime

-- 27-Apr-15
-- parseDDMMMYY :: String -> Maybe Date
v = parseDDMMMYY "7-Apr-15"

g = parseDDMMYY "14/07/2017"

parseDDMMMYY :: String -> Maybe (Integer, Int, Int)
parseDDMMMYY s = toGregorian <$> parseTimeM True defaultTimeLocale "%e-%b-%y" s

parseDDMMYY :: String -> Maybe (Integer, Int, Int)
parseDDMMYY s = toGregorian <$> parseTimeM True defaultTimeLocale "%d/%m/%Y" s

a = map parseAmericanDate ["Nov 27, 2015","Jan 8, 2013"]
-- f =  map formatAsAmericanDate $ a

parseAmericanDate :: String -> Maybe (Integer,Int,Int)
parseAmericanDate s = toGregorian <$> parseTimeM True defaultTimeLocale americanFormat s

americanFormat = "%b %e, %Y"

formatAsAmericanDate :: FormatTime t => t -> String
formatAsAmericanDate = formatTime defaultTimeLocale americanFormat

