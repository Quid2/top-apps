{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.StoreProtocol where

import qualified Data.ByteString as B
import           Network.Top     hiding (RepoProtocol (..), solve)

-- import ZM hiding (solve)
x = runApp def ByType $ recordType (Proxy :: Proxy ValueStoreProtocol)

-- data ValueStoreProtocol = ValueStoreProtocol (StoreProtocol TypedBLOB) deriving (Eq, Ord, Show, Generic, Flat)
type ValueStoreProtocol = StoreProtocol TypedBLOB

data StoreProtocol a
  = Save a
  | Solve (SHAKE128_48 a)
  | Solved (SHAKE128_48 a) a
  deriving (Eq, Ord, Show, Generic, Flat)

instance Model a => Model (StoreProtocol a)

-- |Permanently record a value
saveValue :: (Model a, Flat a) => Config -> a -> IO (SHAKE128_48 TypedBLOB)
saveValue cfg val = save cfg (typedBLOB val)

solveValue ::
     (Model a, Flat a)
  => Network.Top.Config
  -> SHAKE128_48 TypedBLOB
  -> IO (Either String a)
solveValue cfg ref =
  either Left (either (Left . show) Right . untypedBLOB . Right) <$>
  solve cfg ref

-- |Permanently record a value
save :: (Show a, Model a, Flat a) => Config -> a -> IO (SHAKE128_48 a)
save cfg val =
  runApp cfg ByType $ \conn ->
    output conn (Save val) >> return (shake128_48 val)

-- |Retrieve the value identified by the reference
solve ::
     (Flat a, Show a, Model a)
  => Network.Top.Config
  -> SHAKE128_48 a
  -> IO (Either String a)
solve cfg ref =
  runApp cfg ByType $ \conn -> do
    output conn $ Solve ref
    let loop = do
          msg <- input conn
          case msg of
            Solved sref sval
              | sref == ref && shake128_48 sval == ref -> return sval
            _ -> loop
    withTimeout 30 loop
