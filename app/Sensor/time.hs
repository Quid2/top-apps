{-# LANGUAGE ScopedTypeVariables       #-}

-- |Periodically send time
-- BUG: no error logged if temperature reading fails
module Main where

-- import           Data.Maybe
import           Data.Time.Util
-- import           Network.HostName
import           Network.Top             hiding ( Config )
import           Sensor.Model
-- import           System.Exit
-- import           System.Process
-- import           Text.Regex.TDFA
import           Sensor
import           App

main = app "top-time" $ sensor currentTime (milliseconds 500)
