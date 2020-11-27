{-# LANGUAGE ScopedTypeVariables #-}

-- |Periodically send time
module Main where

import App
import Control.Concurrent
import Data.Time.Util
import Network.Top hiding (Config)
import Sensor
import Sensor.Model

main = app $ \_ -> do
  forkIO bool
  sensor currentTime (milliseconds 500)

bool = run $ loop False
 where
  loop b conn = do
    output conn b
    threadDelay (250 * 1000)
    loop (not b) conn
