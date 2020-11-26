module Main where

import App (app)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Stats.Host (registerHostMetrics)

main :: IO ()
main = app $ \store -> do
    registerHostMetrics store
    forever $ do
        threadDelay 1000000