module Util where

import           Network.Top.Util

noRequestLogger _ _ _ = return ()

requestLogger req status maybeL =
  dbg ["Request:", show req, show status, show maybeL]
