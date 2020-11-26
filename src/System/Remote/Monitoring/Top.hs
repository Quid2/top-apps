{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{- | This module lets you periodically flush metrics to Top.

> main = do
>     store <- newStore
>     forkEkgTop defaultTopOptions store

Derived from https://hackage.haskell.org/package/ekg-Top
-}
module System.Remote.Monitoring.Top (
  -- * The Top syncer
  Top,
  topThreadId,
  forkEkgTop,
  TopOptions (..),
  def,
) where

import Control.Concurrent (ThreadId, forkFinally, myThreadId, throwTo)
import Control.Exception ()
-- import Data.Foldable (Foldable (toList))

import Control.Monad
import Data.Default (Default (..))
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO ()
import Flat (Flat, Generic)
import Network.Top (
  ByType (ByType),
  Connection (output),
  Model,
  Proxy (..),
  recordType,
  run,
  runAppForever,
  threadDelay,
 )
import System.Metrics (Store, Value (..), sampleAll)
import System.Metrics.Distribution.Internal (Stats (Stats))

deriving instance Generic Stats

deriving instance Generic Value

instance Flat Stats

instance Model Stats

instance Flat Value

instance Model Value

{- | A handle that can be used to control the Top sync thread.
 Created by 'forkEkgTop'.
-}
newtype Top = Top
  { threadId :: ThreadId
  }

{- | The thread ID of the Top sync thread. You can stop the sync by
 killing this thread (i.e. by throwing it an asynchronous
 exception.)
-}
topThreadId :: Top -> ThreadId
topThreadId = threadId

-- | Options
data TopOptions = TopOptions
  { -- | Data push interval, in secs.
    flushInterval :: !Int
  , -- | Print debug output to stderr.
    debug :: !Bool
  , -- | Prefix to add to all metric names.
    prefix :: !T.Text
  , -- | Suffix to add to all metric names. This is particularly
    -- useful for sending per application stats.
    suffix :: !T.Text
  }

instance Default TopOptions where
  def =
    TopOptions
      { flushInterval = 60
      , debug = False
      , prefix = ""
      , suffix = ""
      }

{- | Create a thread that periodically flushes the metrics in the
 store to a Top file.
-}
forkEkgTop ::
  TopOptions ->
  Store ->
  IO Top
forkEkgTop opts store = do
  me <- myThreadId
  tid <- forkFinally (runSave store opts) $ \r -> do
    case r of
      Left e -> throwTo me e
      Right _ -> return ()
  return $ Top tid

runSave :: Store -> TopOptions -> IO ()
runSave store opts = runAppForever def ByType $ \conn -> do
  let loop = do
        sample <- M.toList <$> sampleAll store
        when (debug opts) $ print sample
        output conn sample
        threadDelay (flushInterval opts * 1000000)
        loop
  loop

-- r :: IO (Either String [()])
-- r = run $ recordType (Proxy :: Proxy Value)

{- | Microseconds since epoch.
 time :: IO Int64
 time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where
   toDouble = realToFrac :: Real a => a -> Double
-}

-- --flushSample :: Metrics.Sample -> TopgerSet -> TopOptions -> IO ()
-- flushSample sample channel opts = do
--   forM_ (M.toList sample) $ \(name, val) -> do
--     time' <- getCurrentTime
--     let newName = dottedPrefix <> name <> dottedSuffix
--         newObj = case val of
--           (Metrics.Counter v) -> object ["timestamp" .= time', newName .= show v]
--           (Metrics.Gauge v) -> object ["timestamp" .= time', newName .= show v]
--           (Metrics.Label v) -> object ["timestamp" .= time', newName .= show v]
--           (Metrics.Distribution v) -> object ["timestamp" .= time', newName .= show v]
--     output sample
--  where
--   isDebug = debug opts

--   dottedPrefix = if T.null (prefix opts) then "" else prefix opts <> "."
--   dottedSuffix = if T.null (suffix opts) then "" else "." <> suffix opts
