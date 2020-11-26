module Main where

import App
import Control.Concurrent
import Control.Monad
import Stats.Host

main = app $ \store -> do
    registerHostMetrics store
    forever $ threadDelay 1000000