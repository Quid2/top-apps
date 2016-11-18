{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
import           Chat.Model         (Message)
import           Control.Concurrent
import           Data.IORef
import qualified Data.Map           as M
import           Network.Top
import           Repo.Disk
import qualified Repo.Types         as R

{-
A simple meta protocol: store the latest value of any type sent via Top and returns it on request.
-}

-- The data type that represents the meta protocol
data LastValueProtocol =
  AskLastValue AbsType                      -- Ask for the last value of the given type
  | LastValue AbsType (BLOB FlatEncoding)   -- Return the last value (flat-encoded)
  deriving (Eq, Ord, Show, Generic, Flat, Model)

-- We run this only once to register our protocol type
register = recordType def (Proxy :: Proxy (LastValueProtocol))

-- Example client
-- We are only interested in receiving LastValue messages so we use a pattern to filter out this particular constructor
client = do
  -- First we send a value of type String
  runClient def ByType $ \conn -> output conn "Just testing!"

  -- Then we retrieve it using the LastValue service
  runClient def $(byPattern [p|LastValue _ _|]) $ \conn -> do
    output conn $ AskLastValue stringType
    LastValue absType value <- input conn
    putStrLn $ "Got it: " ++ show ((unflat . unblob $ value) :: Decoded String)

stringType = absType (Proxy :: Proxy String)
messageType = absType (Proxy :: Proxy Message)

-- The service

-- The service state, a map from types to the last value seen of that type
-- For simplicity we just keep in memory
-- We put it into a IORef so that we can share it across threads
type State = IORef (M.Map AbsType (BLOB FlatEncoding))

main = do
  logLevel DEBUG

  state <- newIORef M.empty

  -- We run two Top connections on separate threads:

  -- The first connection listen for all values exchanged on Top
  -- and stores the last one of every type

  -- We connect using ByAny that will return values of any type
  forkIO $ runClient def ByAny $ \conn -> forever $ do

    -- As the value received can be of any type
    -- it comes as a TypedBLOB, a combination of the type and the binary encoding of the value
    TypedBLOB msgType msgBody <- input conn

    -- show what we got
    -- this shows the unique reference (basically the hash code) of the type
    dbgS (show msgType)

    -- this shows the actual type definition (if the type has been registered)
    -- dbgType msgType

    -- store it in the state
    modifyIORef' state (M.insert msgType msgBody)


  -- The second connection interprets the protocol commands
  -- returning on request the last value detected for every type
  runClient def ByType $ \conn -> forever $ do

    cmd <- input conn
    --dbgS (show cmd)

    case cmd of
      AskLastValue t -> (M.lookup t <$> readIORef state) >>= mapM_ (output conn . LastValue t)
      _ -> return ()


-- Display the definition of a type (if the type is not registered it can be quite slow)
dbgType t = do
  -- Persistent local repository for type definitions
  repo <- dbRepo "/tmp"
  solveType repo def t >>= dbgS . take 200 . prettyShow
  R.close repo
