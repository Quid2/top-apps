{-# LANGUAGE DeriveGeneric ,DeriveAnyClass #-}
-- Data model for reports returned by the quid2.net's Top router
module Model.Report (NestedReport(..),WarpReport(..),ByAnyReport(..),ByTypeReport(..),ByPatternReport(..),ClientReport(..),Time(..)) where
import Data.Time.Util
-- import Data.Time.Clock
import ZM
import Data.Word
import Data.Pattern

data WarpReport = WarpReport {
  version::String
  ,startTime::Time
  ,numOpenedConnections::Word64
  ,numClosedConnections::Word64
  } deriving (Eq,Ord,Show,Generic)

--instance Flat Time
-- instance Model Time
instance Flat WarpReport
instance Model WarpReport

data NestedReport a = NestedReport String a [NestedReport a] deriving (Eq,Ord,Show,Generic,Flat)
instance Model a => Model (NestedReport a)
--instance Flat a => Flat [NestedReport a]

data ByAnyReport = ByAnyReport [ClientReport] deriving (Eq,Ord,Show,Generic,Flat)
instance Model ByAnyReport

data ByTypeReport = ByTypeReport [(AbsType,ClientReport)] deriving (Eq,Ord,Show,Generic,Flat)
instance Model ByTypeReport

data ByPatternReport = ByPatternReport [(AbsType,Pattern,ClientReport)] deriving (Eq,Ord,Show,Generic,Flat)
instance Model ByPatternReport

data ClientReport = ClientReport {clientID::Integer,clientStartTime::Time} deriving (Show,Eq,Ord,Generic,Flat)
instance Model ClientReport


