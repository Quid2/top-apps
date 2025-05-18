{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module NetLog
  ( Log (..),
    Logger (..),
    noLog,
    printLog,
    netLog,
    info,
    warn,
    err,
  )
where

import Data.Text
import Network.Top hiding (Logger, err, info, warn)
import ZM

info, warn, err :: Logger app msg msg msg -> app -> msg -> IO ()
info l app msg = sendLog l $ Info app msg
warn l app msg = sendLog l $ Warning app msg
err l app msg = sendLog l $ Error app msg

data Log app info warn error
  = Info app info -- App working fine, reporting its status
  | Warning app warn -- App might fail, operator intervention required
  | Error app error -- App failed, operator intervention required
  deriving (Eq, Ord, Show, Generic, Flat, Model)

newtype Logger app info warn error = Logger {sendLog :: Log app info warn error -> IO ()}

instance Semigroup (Logger app info warn error) where
  Logger l1 <> Logger l2 = Logger $ \l -> do
    l1 l
    l2 l

noLog :: Logger app info warn error
noLog = Logger $ \_ -> return ()

printLog :: (Show app, Show info, Show warn, Show error) => Logger app info warn error
printLog = Logger print

netLog :: (L app, L info, L warn, L error) => Logger app info warn error
netLog = Logger $ \l -> runApp def ByType $ \conn -> output conn l

type L a = (Model a, Flat a, Show a)
