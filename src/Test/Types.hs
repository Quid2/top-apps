module Test.Types(B8.ByteString,Test(..),Check) where

import qualified Data.ByteString.Char8 as B8

data Test = Test
    { name          :: String
    , timeoutInSecs :: Int
    -- , source :: IO (Either SomeException B8.ByteString)
    , source        :: IO B8.ByteString -- returns content to be checked or throws an IO exception
    , check         :: Check
    }

-- Return Nothing is result is OK or Just <error-message> in case of error
type Check = B8.ByteString -> Maybe String

-- class AsTest t where test :: t -> IO (Maybe String)
-- instance Default where

-- data HTTPTest = HTTPTest {check :: Check, req :: Request}

-- instance Test HTTPTest where
--     test t = do
--         er <- getURL (req t)
--         return $ case er of
--             Left e -> Just (show e)
--             Right c -> check t c
