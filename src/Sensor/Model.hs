{-# Language DeriveGeneric , DeriveAnyClass #-}
module Sensor.Model where

import ZM

-- A generic sensor reading
data SensorReading unit place = SensorReading {reading::unit,location::place}
  deriving (Eq, Ord, Read, Show, Generic, Flat, NFData)

-- A location that Google Maps can make sense of, such as: GoogleMapsLocation "The+White+House"
data GoogleMapsLocation = GoogleMapsLocation String
       deriving (Eq, Ord, Read, Show, Generic, Model, Flat, NFData)

-- This means just what it seems to mean
--data Temperature t = Temperature t deriving (Eq, Ord, Read, Show, Generic)

data Celsius = Celsius Float deriving (Eq, Ord, Read, Show, Generic, Model, Flat, NFData)

data Humidity = Humidity Percentage deriving (Eq, Ord, Read, Show, Generic, Model, Flat, NFData)

-- |A 0..100 value
data Percentage = Percentage Float deriving (Eq, Ord, Read, Show, Generic, Model, Flat, NFData)

-- instance (Flat, NFData a,Flat, NFData b) => Flat, NFData (SensorReading a b)
instance (Model a,Model b) => Model (SensorReading a b)



