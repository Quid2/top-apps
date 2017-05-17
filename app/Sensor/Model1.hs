{-# Language DeriveGeneric ,DeriveAnyClass #-}
module Sensor.Model1 where

import ZM

-- A rather asinine data model
data MySensor = MySensor Int -- These are Celsius by the way!
              deriving (Eq, Ord, Read, Show, Generic, Flat, Model)
