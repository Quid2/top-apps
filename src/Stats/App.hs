{-# LANGUAGE OverloadedStrings #-}

module Stats.App (registerAppMetrics) where

import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import qualified GHC.Stats as Stats
import System.Metrics (
    Store,
    Value (Counter, Gauge),
    registerGroup,
 )

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

-- | Convert nanoseconds to milliseconds.
nsToMs :: Int64 -> Int64
nsToMs s = round (realToFrac s / (1000000.0 :: Double))
