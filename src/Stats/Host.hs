{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stats.Host (registerHostMetrics) where

{-
See also:

https://hackage.haskell.org/package/proc
memory/cpu for all processes

https://hackage.haskell.org/package/disk-free-space

-}

import qualified Data.HashMap.Strict as M
import Data.Maybe (fromJust)
import System.Exit (ExitCode (ExitSuccess))
import System.Linux.Proc.MemInfo (
    MemInfo (memAvailable, memTotal),
    readProcMemInfo,
 )
import System.Metrics (Store, Value (Gauge), registerGroup)
import System.Process (readProcessWithExitCode)
import Text.Regex.TDFA ((=~))

registerHostMetrics :: Store -> IO ()
registerHostMetrics store = do
    registerGroup
        ( M.fromList
            [ ("host.mem_available", gauge memAvailable)
            , ("host.mem_total", gauge memTotal)
            ]
        )
        readProcMemInfo
        store
    registerGroup
        ( M.fromList
            [ ("host.temperature_celsius", Gauge . round)
            ]
        )
        cpuTemperature
        store

gauge acc = Gauge . fromIntegral . either (const 0) acc

-- |Returns CPU temperature (in Linux systems with working 'sensors')
cpuTemperature :: IO Float
cpuTemperature = do
    (ExitSuccess, out, _err) <- readProcessWithExitCode "sensors" [] ""
    return . fromJust . parseTemperature $ out

parseTemperature :: String -> Maybe Float
parseTemperature str =
    let (_, _, _, matches) :: (String, String, String, [String]) =
            str =~ ("Core 0:[ ]+([+-][1-9][0-9]*[.][0-9]+).C" :: String)
     in if length matches == 1
            then
                let n = head matches
                 in Just (read (if head n == '+' then tail n else n) :: Float)
            else Nothing
