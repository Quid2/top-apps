{-# LANGUAGE DeriveGeneric ,DeriveAnyClass #-}
import Network.Top

-- |Send a message and then print out all messages received
main = runApp def ByType $ \conn -> do
  logLevel DEBUG
  output conn Message {fromUser="robin",content=TextMessage "Hello!"}
  loop conn
    where loop conn = input conn >>= print >> loop conn

-- Data model for a very simple chat system
data Message = Message {
     fromUser::String
    ,content::Content
    } deriving (Eq, Ord, Read, Show, Generic , Flat, Model)

data Content =
    TextMessage String

    | HTMLMessage String
    deriving (Eq, Ord, Read, Show, Generic, Flat, Model)
