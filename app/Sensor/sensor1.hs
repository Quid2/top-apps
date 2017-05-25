{-# LANGUAGE DeriveGeneric #-}
import Network.Top

import Sensor.Model1

main = runAppForever def ByType loop
     where
       loop conn = do
        reading <- readSensor
        output conn reading
        threadDelay (minutes 3)
        loop conn

readSensor :: IO MySensor
readSensor = return $ MySensor 15
