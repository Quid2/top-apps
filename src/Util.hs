module Util (noRequestLogger, msecs, secs, minutes, periodically, voidFork) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Network.Top.Util (dbg)
import Prelude

voidFork = void . forkIO

noRequestLogger :: (Monad m) => p1 -> p2 -> p3 -> m ()
noRequestLogger _ _ _ = return ()

requestLogger :: (MonadIO m, Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> m ()
requestLogger req status maybeL =
  dbg ["Request:", show req, show status, show maybeL]

msecs :: (Num a) => a -> a
msecs t = t * 1000

secs :: (Num a) => a -> a
secs t = t * msecs 1000

minutes :: (Num a) => a -> a
minutes t = t * secs 60

periodically :: Int -> IO a -> IO ()
periodically numSeconds action = voidFork $ forever $ do
  action
  threadDelay $ secs numSeconds
