module Util where

import Network.Top.Util

noRequestLogger req status maybeL = return ()

requestLogger req status maybeL = dbg ["Request:",show req,show status,show maybeL]
