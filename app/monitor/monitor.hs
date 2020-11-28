{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
import qualified Data.Text.IO as T
import Network.Top
import Network.Top.Pipes
import qualified Pipes as P
import qualified Pipes.Prelude as P
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import RIO.Time
import Stats.Host (registerHostMetrics)
import System.Metrics (Sample, Value (..))
import System.Remote.Monitoring.Top (runSample)
import System.Time.Extra (
  showDuration,
 )
import Text.Printf (printf)

data ServiceStatus = OK | WARN Text | FAIL Text deriving (Show)

-- data Env = Env {services :: !Services} deriving (Show)

-- newEnv :: Env
-- newEnv = Env HM.empty

type Status = (UTCTime, Sample)

type Services = HM.HashMap Service Status

data Service = Service {srvName, srvHost, srvId :: Text} deriving (Eq, Show, Ord, Generic, Hashable)

-- class TShow a where tshow :: a -> Te
-- instance TShow Service where tshow s = T.concat [srvName s, "-", srvId s, "@", srvHost s]

getService s = (\name pid host -> Service name host pid) <$> lbl "app.name" s <*> lbl "app.pid" s <*> lbl "host.name" s

t :: IO ()
t = main

main :: IO ()
main = app $ \_ -> do
  -- let env = newEnv
  -- BUG: If a connection fails, this whole pipe will be restarted (and state is lost) NOT REALLY, CONNECTION IS PRESERVED
  -- BUG: nothing is displayed if nothing is received (so also failure of contacting network is undetected)
  -- TODO: send warning via email/sms
  runSample $ \conn -> runEffect $ pipeIn conn >-> updateServices HM.empty >-> P.mapM status >-> P.mapM report >-> P.drain

report ss = do
  putStrLn "\nReport:"
  mapM_ T.putStrLn $ HM.mapWithKey (\s st -> T.unwords [tshow s, ":", tshow st]) ss

status ss =
  do
    now <- getCurrentTime
    let since = diffUTCTime now
    return $
      HM.map
        ( \v@(t, s) ->
            let memUsedM = gge "app.bytes_used" s
                diff = since t
             in case (v, memUsedM) of
                  (time, _) | diff > maxWait -> FAIL (T.unwords ["last seen", tshow diff, "ago"])
                  (_, Just memUsed) | memUsed > maxMem -> WARN (T.unwords ["memory used", mem memUsed])
                  _ -> OK
        )
        ss
 where
  maxWait = secs 120
  maxMem = mb 20
  secs n = toEnum $ n * 1000000000000
  mb = (1000000 *)

getServices = HM.keys

updateServices ss = do
  sample <- HM.fromList <$> await
  let Just s = getService sample
  now <- getCurrentTime
  let ss' = HM.insert s (now, sample) ss
  yield ss'
  updateServices ss'

counter tot = do
  v <- await
  let tot' = tot + 1
  yield (tot', v)
  counter tot'

showSample :: Sample -> Text
showSample s =
  let name = (\name pid host -> T.concat [name, "-", pid, "@", host]) <$> lbl "app.name" s <*> lbl "app.pid" s <*> lbl "host.name" s
      temp = (`T.append` "C") . T.pack . show <$> gge "host.temperature_celsius" s
      memUsed = mem <$> gge "app.bytes_used" s
      runTime = T.pack . showDuration . (/ 1000) . fromIntegral <$> ctr "app.wall_ms" s
      out = (\name temp time mem -> T.unwords [name, temp, "running for", time, "using", mem]) <$> name <*> temp <*> runTime <*> memUsed
   in chk s out

getHost :: Sample -> Text
getHost s =
  let name = lbl "host.name" s
      temp = (`T.append` "C") . T.pack . show <$> gge "host.temperature_celsius" s
      memAvail = gge "host.mem_available" s
      memTot = gge "host.mem_total" s
      -- memPerc = div <$> memAvail <*> ((* 100) <$> memTot)
      out = (\name temp avl tot -> T.unwords [name, temp, "avail mem", mem avl, "total mem", mem tot]) <$> name <*> temp <*> memAvail <*> memTot
   in chk s out

chk :: Show a => a -> Maybe Text -> Text
chk s = fromMaybe (T.unwords ["Unexpected", T.pack . show $ s])

lbl :: Text -> Sample -> Maybe Text
lbl name s = HM.lookup name s >>= unLabel

gge, ctr :: Text -> Sample -> Maybe Int64
gge name s = HM.lookup name s >>= unGauge
ctr name s = HM.lookup name s >>= unCounter

unLabel :: Value -> Maybe Text
unLabel (Label l) = Just l
unLabel _ = Nothing

unGauge :: Value -> Maybe Int64
unGauge (Gauge v) = Just v
unGauge _ = Nothing

unCounter :: Value -> Maybe Int64
unCounter (Counter v) = Just v
unCounter _ = Nothing

mem :: Int64 -> Text
mem m = let (v, u) = mem_ (fromIntegral m) in T.pack (printf "%.1f" v) <> u

mem_ :: Double -> (Double, Text)
mem_ m
  | m < 1000 = (m, "B")
  | m < 1000000 = (m / 1000, "KB")
  | m < 1000000000 = (m / 1000000, "MB")
  | otherwise = (m / 1000000000, "GB")