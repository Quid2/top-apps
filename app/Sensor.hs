{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sensor (
  sensor,
) where

import Data.Bifunctor
import Data.Either
import Network.Top hiding (Config)

-- sensor0 read minInterval = do
--   r <- read
--   dbgShow r

-- | Periodically read a sensor and send over its  value on the corresponding channel, if the value of the reading changes or it is the first time that the sensor is read.
sensor ::
  forall a.
  (NFData a, Flat a, Model a, Show a, Eq a) =>
  IO a ->
  Int ->
  IO ()
sensor read minInterval = runA $ \conn -> do
  dbgS "started sensor"
  let io = first show <$> strictTry read
  let out v = when (isRight v) $ output conn (let Right o = v in o)
  let loop v = do
        threadDelay minInterval
        v1 <- io
        --dbgShow v1
        when (v1 /= v) $ out v1
        loop v1
  v <- io
  dbgShow v
  out v
  loop v

runA :: forall a r. (Model a, Flat a, Show a) => App a r -> IO r
runA = runAppForever def ByType

-- run app = runApp def ByType app
-- dbgShow = dbgS . show
-- dbgSh = dbgS . show
-- dbgShow _ = return ()
