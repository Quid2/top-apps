{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Example program that continously computes the mean of a list of
 numbers.
-}
module Ekg where

import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import RIO
import System.Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Label as Label
import System.Remote.Monitoring.Top

{-

Host info:
https://hackage.haskell.org/package/system-linux-proc
available memory

https://hackage.haskell.org/package/proc
memory/cpu for all processes

https://hackage.haskell.org/package/disk-free-space

-}

-- 'sum' is using a non-strict lazy fold and will blow the stack.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: Fractional a => [a] -> a
mean xs = sum' xs / fromIntegral (length xs)

app op = do
    store <- newStore
    registerGcMetrics store
    forkEkgTop def{flushInterval = 60, debug = True} store
    op store

t :: IO ()
t = app $ \store -> do
    requests <- createCounter "myapp.request_count" store
    -- Every time we receive a request:
    Counter.inc requests
    counter <- createCounter "iterations" store
    label <- createLabel "args" store
    Label.set label "some text string"
    event <- createDistribution "runtime" store

    let loop n = do
            t <- timed $ evaluate $ mean [1 .. n]
            Distribution.add event t
            threadDelay 2000
            Counter.inc counter
            loop n
    loop 1000000

timed :: IO a -> IO Double
timed m = do
    start <- getTime
    m
    end <- getTime
    return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime