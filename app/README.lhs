Example applications, in Haskell, for [Top](https://github.com/tittoassini/top).

Developing distributed applications that use *Top*, the type oriented protocol, is straightforward:
* define a data model, that's just one or more serialisable Haskell data types
* automatically derive instances of the *Flat* (serialisation) and *Model* (introspection) classes
* connect to one or more typed channels and send/receive values to/from any other client using the same types

 #### Example: collecting data from distributed sensors

Suppose that you want to keep an eye on your house while at work, and in particular check for possible fires.

A `sensor` program that reads the temperature from a sensor and broadcast it using *Top* would be:

!source top-apps:app/Sensor/sensor0.hs

We will also need a `sensor-check` program to collect the data and print it out: 

!source top-apps:app/Sensor/sensor-check0.hs

It could not be easier than this.

There is just a little issue, as we mentioned, in *Top* there is a channel for every possible serialisable type.

Our data will therefore travel over the 'Int' channel.

Obviously other users might use this channel for their own purposes and then it would be rather hard to distinguish our Ints from those of anyone else and whose meaning has nothing to do at all with temperature readings.

Time to develop a slightly more sophisticated data model.

As it will need to be shared between the `sensor` and the `sensor-check` program, we will put the data model in a separate module:

!source top-apps:app/Sensor/Model1.hs

Our data type needs to be an instance of the *Flat* (serialisation) and *Model* (introspection) classes, the instances are automatically derived, provided that the type derives `Generic`.

The `sensor` program has barely changed:

!source top-apps:app/Sensor/sensor1.hs

and similarly `sensor-check`:
!source top-apps:app/Sensor/sensor-check1.hs

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

!source top-apps:app/meta.hs

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
