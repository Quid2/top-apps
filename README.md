
Example applications, in Haskell, for [Top](https://github.com/tittoassini/top).

Developing distributed applications that use *Top*, the type oriented protocol, is straightforward:
* define a data model, that's just one or more serialisable Haskell data types
* automatically derive instances of the *Flat* (serialisation) and *Model* (introspection) classes
* connect to one or more typed channels and send/receive values to/from any other client using the same types

#### Example: collecting data from distributed sensors

Suppose that you want to keep an eye on your house while at work, and in particular check for possible fires.

A `sensor` program that reads the temperature from a sensor and broadcast it using *Top* would be:

```haskell
-- Broadcast a temperature reading every few minutes

-- Import the Quid2 API
import Network.Top

{-
runAppForever opens a connection to a Top channel and keeps it alive even across transient network failures.

The parameters are:
def:
The default Top network configuration, no need to change this.

ByType:
The kind of routing logic that we want to use on this connection.
'ByType' indicates that we want to receive all messages of a given type.
We do not need to indicate explicitly the type, as it is implicitly specified by the type of 'loop'.

loop:
The application code, it is passed the connection as soon as it is opened.
It uses `output` to send out the sensor reading.
-}
main = runAppForever def ByType loop
     where
       -- |The sensor reading loop
       loop conn = do
        -- Read the sensor value
        reading <- readSensor
        -- Send the value on the 'Int' channel
        output conn reading

        -- Wait and repeat
        threadDelay (minutes 3)
        loop conn

-- |Fake sensor reading operation
readSensor :: IO Int
readSensor = return 15
```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/Sensor/sensor0.hs)</sup>

We will also need a `sensor-check` program to collect the data and print it out:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Network.Top

-- |Collect sensor data and give warnings if needed
main = runAppForever def (ByType::ByType Int) loop
     where
       loop conn = do
         temperature :: Int <- input conn
         print $ show temperature ++ " Celsius"
         when (temperature > 50) $ print "ALARM, HOUSE ON FIRE!!!!"
         loop conn

```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/Sensor/sensor-check0.hs)</sup>

It could not be easier than this.

There is just a little issue, as we mentioned, in *Top* there is a channel for every possible serialisable type.

Our data will therefore travel over the 'Int' channel.

Obviously other users might use this channel for their own purposes and then it would be rather hard to distinguish our Ints from those of anyone else and whose meaning has nothing to do at all with temperature readings.

Time to develop a slightly more sophisticated data model.

As it will need to be shared between the `sensor` and the `sensor-check` program, we will put the data model in a separate module:

```haskell
{-# Language DeriveGeneric ,DeriveAnyClass #-}
module Sensor.Model1 where

import ZM

-- A rather asinine data model
data MySensor = MySensor Int -- These are Celsius by the way!
              deriving (Eq, Ord, Read, Show, Generic, Flat, Model)
```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/Sensor/Model1.hs)</sup>

Our data type needs to be an instance of the *Flat* (serialisation) and *Model* (introspection) classes, the instances are automatically derived, provided that the type derives `Generic`.

The `sensor` program has barely changed:

```haskell
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
```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/Sensor/sensor1.hs)</sup>

and similarly `sensor-check`:
```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Network.Top

import Sensor.Model1

-- |Collect sensor data and give warnings if needed
main = runAppForever def ByType loop
     where
       loop conn = do
         MySensor temperature <- input conn
         print $ show temperature ++ " Celsius"
         when (temperature > 50) $ print "ALARM, HOUSE ON FIRE!!!!"
         loop conn

```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/Sensor/sensor-check1.hs)</sup>

So now we are transferring our data on the `MySensor` channel, that makes a bit more sense, at least to us.

But what if we have more than one kind of sensors or if our sensors are in different locations, maybe a temperature sensor at home and a humidity sensor in the allotment garden?

More importantly, what if our friends also want to access their sensors?

There might be some value in a versatile and open distributed sensor network but to support it we need a much richer and shareable model that others might be willing to adopt.

And here we come to the main point about *Top*: to facilitate large scale data exchange by progressively converging on a shared lexicon of data types.

Before inventing your own types you should check those [already defined](http://quid2.org/app/ui).

The more you reuse existing concepts, the greater the value of your data and programs.

#### Establish Provenance and Preserve Data Integrity

It's all fine and dandy till you receive a fire alarm and run to your house just to discover that your friend Bob, a notorious prankster, has sent you fake readings pretending to be your faithful temperature sensor.

Remember, *Top* channels are public, anyone can send anything.

A simple way to establish provenance, is to sign your values.

We start by defining a type for signed values:

```haskell
-- |A signed value
data Signed value signature = Signed value signature
```

Then one or more types to represent specific signature algorithms, for example [Ed25519](http://ed25519.cr.yp.to/):

```haskell
-- |An Ed255619 signature
data Ed255619 = Ed255619 [Word8] deriving (Eq, Ord, Read, Show, Generic)
```
By embedding our values in `Signed Ed255619`, we can avoid being pranked again.

For an example, see [`signed`](app/signed.hs).

As a bonus, this will also guarantee the messages' integrity, if anyone had tampered with them in any way, the signature won't match.

A similar procedure can be followed to add encryption, data compression or any other required feature in a flexible, autonomous and incremental way.

#### Meta Services

Rather than adding these extra features on a case by case way, we can also implement 'Meta' services that add a specific feature for all possible types.

For an example see:

```haskell
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
  runApp def ByType $ \conn -> output conn "Just testing!"

  -- Then we retrieve it using the LastValue service
  runApp def (byPattern $(patternE [p|LastValue _ _|])) $ \conn -> do
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
  forkIO $ runApp def ByAny $ \conn -> forever $ do

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
  runApp def ByType $ \conn -> forever $ do

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
```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/meta.hs)</sup>

#### Examples

Have a look at some examples of clients/bots:
* [`hello`](app/hello.hs)
   * Simplest possible self-contained agent
* [`chat-history`](app/Chat/chat-history.hs)
   * A simple bot that will store all messages conforming to a simple chat data model and will send them back on request 
* [`chat`](app/Chat/chat.hs)
   * Basic end-user client (multiple channels, usage of Pipes). 
* [`signed`](app/signed.hs)
   * Using cryptographic signatures to establish provenance and preserve data integrity
* [`meta`](app/meta.hs)
   * Simple meta protocol, stores the latest value of any type and returns it on request

To run the corresponding executable from command line:
`stack exec top-<example_name>`

For example: `stack exec top-hello`.

#### API Documentation

Start with:
* [Network.Top.Run](https://github.com/tittoassini/top/blob/master/src/Network/Top/Run.hs)
* [Network.Top.Pipes](https://github.com/tittoassini/top/blob/master/src/Network/Top/Pipes.hs)
* [Network.Top.Types](https://github.com/tittoassini/top/blob/master/src/Network/Top/Types.hs)

### Installation

 This is not a library, it is a set of examples, so you should clone it or fork it and take it as a starting point for developing *Top* applications.

### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.1.
