{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (app)
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

-- data Status = Running | Stopped Sample UTCTime |

data Env = Env {services :: !Services} deriving (Show)

newEnv :: Env
newEnv = Env HM.empty

type Status = (UTCTime, Sample)

type Problem = String

type Services = HM.HashMap Service Status

data Service = Service {srvName, srvHost, srvId :: Text} deriving (Eq, Show)

t :: IO ()
t = main

-- main :: IO ()
-- main = app $ \_ -> do
--   env <- newEnv
--   runSample loop
--  where
--   loop conn = do
--     sample <- HM.fromList <$> input conn
--     print sample
--     putStrLn . T.unpack . showSample $ sample
--     putStrLn . T.unpack . getHost $ sample
--     loop conn

main :: IO ()
main = app $ \_ -> do
  let env = newEnv
  -- If a connection fails, this whole pipe will be restarted
  runSample $ \conn -> runEffect $ pipeIn conn >-> P.map (showSample . HM.fromList) >-> P.take 2 >-> counter 0 >-> P.print

x :: IO ()
x = run $ \conn -> runEffect $ pipeIn conn >-> P.take 3 >-> P.map (show :: Bool -> String) >-> P.drain

counter tot = do
  v <- await
  let tot' = tot + 1
  yield (tot', v)
  counter tot'

--  where
--   loop conn = do
--     sample <- HM.fromList <$> input conn
--     print sample
--     putStrLn . T.unpack . showSample $ sample
--     putStrLn . T.unpack . getHost $ sample

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

class TShow a where tshow :: a -> Text
instance TShow Service where tshow s = T.concat [srvName s, "-", srvId s, "@", srvHost s]

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