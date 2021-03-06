{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Example program that continously computes the mean of a list of
 numbers.
-}
module App (app, timed) where

import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import RIO
import Stats.App (registerAppMetrics, registerAppNames)
import Stats.Host (registerHostName)
import System.Metrics (Store, createLabel, newStore)
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Label as Label
import System.Remote.Monitoring.Top (
    TopOptions (debug, flushInterval),
    def,
    forkEkgTop,
 )

app :: (Store -> IO b) -> IO b
app op = do
    store <- newStore
    registerAppMetrics store
    registerAppNames store
    registerHostName store
    forkEkgTop def{flushInterval = 60, debug = False} store
    op store

-- host :: IO ()
-- host = app $ \store -> do
--     registerHostMetrics store

-- t :: IO ()
-- t = app $ \store -> do
--     requests <- createCounter "myapp.request_count" store
--     -- Every time we receive a request:
--     Counter.inc requests
--     counter <- createCounter "iterations" store
--     label <- createLabel "args" store
--     Label.set label "some text string"
--     event <- createDistribution "runtime" store

--     let loop n = do
--             t <- timed $ evaluate $ mean [1 .. n]
--             Distribution.add event t
--             threadDelay 2000
--             Counter.inc counter
--             loop n
--     loop 1000000

timed :: IO a -> IO Double
timed m = do
    start <- getTime
    m
    end <- getTime
    return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime