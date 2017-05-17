import           Chat.Model
import           Data.Int
import           Data.Time.Util
import           Data.Word
import           Network.Top
import           Sensor.Model
import           Repo.Types                           hiding (Repo (..))

x = do
  Right ts <- knownTypes def
  -- putStr $ prettyShow ts
  print $ lookup "Bool" $ map (\(_,adt) -> (prettyShow . declName $ adt,prettyShow adt)) ts

y = let tm = absTypeModel (Proxy :: Proxy Bool)
    in typeDefinition (typeEnv tm) (typeName tm)


-- Recover all types and write code needed to push them back in repo
k = do
  Right ts <- knownTypes def
  mapM_ putStrLn $ map (sh . snd) ts
  where
    sh adt = let name = prettyShow $ declName adt
                 vars = unwords $ take (fromIntegral $ declNumParameters adt) $ repeat "()"
             in unwords ["  recordType def (Proxy::Proxy (",name,vars,"))"]

toDrop = [ "Tuple2"
         , "Bits8"
         , "Bits23"
         , "Bits7"
         , "Sign"
         , "Bit"
         , "Array"
         , "List"
         , "Msg"
         , "LastValueProtocol"
         , "HostPort"
         , "Pattern","Content"
         ]
toChange = [("Unit","()")
           ,(" IEEE_754_binary32","Float")
           ,(" IEEE_754_binary64","Double")
           ]

t = recordType def (Proxy::Proxy Int)

main = do
  recordType def (Proxy::Proxy ( ()  ))
  recordType def (Proxy::Proxy ( Float  ))
  recordType def (Proxy::Proxy ( Double  ))
  recordType def (Proxy::Proxy ( Char  ))
  recordType def (Proxy::Proxy ( MostSignificantFirst () ))
  recordType def (Proxy::Proxy ( Int64  ))
  recordType def (Proxy::Proxy ( Word8  ))
  recordType def (Proxy::Proxy ( Identifier  ))
  recordType def (Proxy::Proxy ( User  ))
  recordType def (Proxy::Proxy ( ZigZag () ))
  recordType def (Proxy::Proxy ( HostAddress () ))
  recordType def (Proxy::Proxy ( WebSocketAddress () ))
  recordType def (Proxy::Proxy ( Message  ))
  recordType def (Proxy::Proxy ( Word32  ))
  recordType def (Proxy::Proxy ( ByType () ))
  recordType def (Proxy::Proxy ( Type () ))
  recordType def (Proxy::Proxy ( ByPattern () ))
  recordType def (Proxy::Proxy ( BLOB () ))
  recordType def (Proxy::Proxy ( NonEmptyList () ))
  recordType def (Proxy::Proxy ( Word  ))
  recordType def (Proxy::Proxy ( LeastSignificantFirst () ))
  recordType def (Proxy::Proxy ( SensorReading () () ))
  recordType def (Proxy::Proxy ( ChannelSelectionResult () ))
  recordType def (Proxy::Proxy ( Maybe () ))
  recordType def (Proxy::Proxy ( ADTRef () ))
  recordType def (Proxy::Proxy ( AbsRef  ))
  recordType def (Proxy::Proxy ( Bool  ))
  recordType def (Proxy::Proxy ( IP4Address  ))
  recordType def (Proxy::Proxy ( UnicodeLetter  ))
  recordType def (Proxy::Proxy ( RepoProtocol  ))
  recordType def (Proxy::Proxy ( Word16  ))
  recordType def (Proxy::Proxy ( Word64  ))
  recordType def (Proxy::Proxy ( ADT () () () ))
  recordType def (Proxy::Proxy ( ConTree () () ))
  recordType def (Proxy::Proxy ( Int  ))
  recordType def (Proxy::Proxy ( UnicodeSymbol  ))
  recordType def (Proxy::Proxy ( Either () () ))
  recordType def (Proxy::Proxy ( Celsius  ))
  recordType def (Proxy::Proxy ( Word7  ))
  recordType def (Proxy::Proxy ( SHA3_256_6 () ))
  recordType def (Proxy::Proxy ( SensorReading () () ))
  recordType def (Proxy::Proxy ( Filler  ))
  recordType def (Proxy::Proxy ( FlatEncoding  ))
  recordType def (Proxy::Proxy ( SocketAddress () ))
  recordType def (Proxy::Proxy ( SensorReading () () ))
  recordType def (Proxy::Proxy ( Subject  ))
  recordType def (Proxy::Proxy ( WildCard  ))
  recordType def (Proxy::Proxy ( Time  ))
  recordType def (Proxy::Proxy ( UnicodeLetterOrNumberOrLine  ))
  recordType def (Proxy::Proxy ( UTF8Encoding  ))
  recordType def (Proxy::Proxy ( PreAligned () ))
  recordType def (Proxy::Proxy ( ByAny () ))
