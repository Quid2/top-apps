 {-# LANGUAGE OverloadedStrings           #-}
module Model.Report.Util(
  --getByType,getServerState
  reportURL,printReport,byAnyReport,byTypeReport,byPatternReport
  ) where
import ZM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Model.Report
import Control.Monad
import Network.Top.Types(Config(..),def,cfgIP,cfgPort)

reportURL cfg = concat["http://",cfgIP cfg,":",show (cfgPort cfg),"/report"]

bytes = B.unpack . unblob

byAnyReport = concatMap (\(ByAnyReport vs) -> vs) . byAnyReport_

byAnyReport_ (NestedReport n (TypedBLOB t b) ss) =
   if (t == byAnyReportType)
   then [dec (bytes b)::ByAnyReport]
   else concatMap byAnyReport_ ss

byTypeReport = concatMap (\(ByTypeReport vs) -> vs) . byTypeReport_

byTypeReport_ (NestedReport n (TypedBLOB t b) ss) =
   if (t == byTypeReportType)
   then [dec (bytes b)::ByTypeReport] 
   else concatMap byTypeReport_ ss

-- Scan NestedReport for ByPatternReport 
byPatternReport = concatMap (\(ByPatternReport vs) -> vs) . byPatternReport_

byPatternReport_ (NestedReport n (TypedBLOB t b) ss) =
   if (t == byPatternReportType)
   then [dec (bytes b)::ByPatternReport] -- found
   else concatMap byPatternReport_ ss    -- we keep searching

printReport (NestedReport n (TypedBLOB t b) ss) = do
  let bs = bytes b
  putStrLn n
  print t
  print ("warpReportType",warpReportType)
  when (t == warpReportType) $ print (dec bs::WarpReport)
  when (t == byAnyReportType) $ print (dec bs::ByAnyReport)
  when (t == byTypeReportType) $ print (dec bs::ByTypeReport)
  when (t == byPatternReportType) $ print (dec bs::ByPatternReport)
  when (t == echoReportType) $ print (dec bs::[ClientReport])
  mapM_ printReport ss

dec bs = let Right a = unflat bs in a

warpReportType = absType (Proxy::Proxy WarpReport)
stringType = absType (Proxy::Proxy String)
intType = absType (Proxy::Proxy Int)
byAnyReportType = absType (Proxy::Proxy ByAnyReport)
byTypeReportType = absType (Proxy::Proxy ByTypeReport)
byPatternReportType = absType (Proxy::Proxy ByPatternReport)
echoReportType = absType (Proxy::Proxy [ClientReport])

