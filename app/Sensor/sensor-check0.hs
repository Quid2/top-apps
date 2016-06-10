{-# LANGUAGE ScopedTypeVariables #-}
import Network.Top

-- |Collect sensor data and give warnings if needed
main = runClientForever def (ByType::ByType Int) loop
     where
       loop conn = do
         Just temperature :: Maybe Int <- input conn
         print $ show temperature ++ " Celsius"
         when (temperature > 50) $ print "ALARM, HOUSE ON FIRE!!!!"
         loop conn

