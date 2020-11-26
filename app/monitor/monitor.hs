module Main where

import App (app)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Network.Top
import Stats.Host (registerHostMetrics)

import System.Remote.Monitoring.Top

main :: IO ()
main = app $ \store -> do
    registerHostMetrics store
    runSample $ \conn -> do
        sample <- input conn
        print sample