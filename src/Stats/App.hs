{-# LANGUAGE OverloadedStrings #-}

module Stats.App (registerAppMetrics, registerAppNames) where

import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import qualified Data.Text as T
import qualified GHC.Stats as Stats
import System.Environment (getProgName)
import System.Metrics
import qualified System.Metrics.Label as Label
import System.Posix.Process (getProcessID)

registerAppMetrics :: Store -> IO ()
registerAppMetrics =
    registerGroup
        ( M.fromList
            [ ("app.wall_ms", Counter . nsToMs . Stats.elapsed_ns)
            , -- , ("app.max_bytes_used", Gauge . fromIntegral . Stats.max_live_bytes)
              ("app.bytes_used", Gauge . fromIntegral . Stats.gcdetails_live_bytes . Stats.gc)
            ]
        )
        Stats.getRTSStats

registerAppNames :: Store -> IO ()
registerAppNames store = do
    appName <- createLabel "app.name" store
    appID <- createLabel "app.pid" store
    name <- getProgName
    id <- getProcessID
    -- Label.set appName $ T.concat [T.pack name, "-", T.pack . show $ id, "@", T.pack host]
    Label.set appName $ T.pack name
    Label.set appID $ T.pack . show $ id

-- | Convert nanoseconds to milliseconds.
nsToMs :: Int64 -> Int64
nsToMs s = round (realToFrac s / (1000000.0 :: Double))
