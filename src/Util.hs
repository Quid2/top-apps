module Util where

import           Control.Monad.IO.Class
import           Network.Top.Util       (dbg)

noRequestLogger :: Monad m => p1 -> p2 -> p3 -> m ()
noRequestLogger _ _ _ = return ()

requestLogger :: (MonadIO m, Show a1, Show a2, Show a3) => a1 -> a2 -> a3 -> m ()
requestLogger req status maybeL =
  dbg ["Request:", show req, show status, show maybeL]


