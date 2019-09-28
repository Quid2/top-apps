{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-
Generic storage for zm values.
-}
import           Data.Bifunctor
import qualified Data.ByteString     as B
import           Model.StoreProtocol
import           Network.Top         hiding (RepoProtocol (..))
import           Quid2.Util.Service
import           System.Directory
import           System.FilePath
import           System.IO
import           ZM

main = initService "top-store" setup

setup :: Quid2.Util.Service.Config () -> IO ()
setup cfg = do
  -- updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO
  logLevelOut DEBUG stdout

  let vdir = (stateDir cfg) </> "values"
  createDirectoryIfMissing False vdir

  runAgent (agent vdir)

agent dir = do
  msg <- await
  dbg ["MSG",take 100 $ show msg]
  case msg of
    Solve ref -> do
      eadt <- lift $ solveRef ref
      case eadt of
        Left _    -> return ()
        Right val -> yield $ Solved ref val

    Save val ->
      let bval = flat val
          bref = shake128_48_BS bval
          file = valFile bref
      in lift $ do
         r <- solveRef bref
         case r of
           Left _ -> B.writeFile file bval
           Right pv -> when (pv/=val) $ err ["The unthinkable just happened: hash clash between",show pv,"and",show val]

    _ -> return ()

  agent dir

  where
    solveRef :: SHAKE128_48 TypedBLOB -> IO (Either String TypedBLOB)
    solveRef bref = do
      let file = valFile bref
      f <- doesFileExist file
      if f
        then first show . unflat <$> B.readFile file
        else return . Left $ "No file: "++file

    valFile bref = dir </> prettyShow bref

-- Move in top library
runAgent agent = runAppForever def ByType $ \conn -> runEffect $ pipeIn conn >-> agent >-> pipeOut conn
