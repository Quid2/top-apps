{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Types (Test (..), test, testIf, runTest) where

import Network.Top (seconds)
import RIO (SomeException, try, tryAnyDeep)
import System.Timeout (timeout)

{-
>>> runTest $ test "x" (return "x") (\r -> if r == "s" then Nothing else Just "Bad")
Just "x: Bad"
-}

data Test = forall r. Test
  { name :: String,
    timeoutInSecs :: Int,
    check ::
      r ->
      Maybe String,
    source ::
      IO
        r
  }

testIf :: String -> (r -> Bool) -> IO r -> Test
testIf name chk = test name (\r -> if chk r then Nothing else Just (name <> " failed"))

test :: String -> (r -> Maybe String) -> IO r -> Test
test name = Test name 30

apply :: Test -> IO (Maybe String)
apply (Test {source = src, check = chk}) = chk <$> src

runTest :: Test -> IO (Maybe String)
runTest (Test {name = title, timeoutInSecs = tout, source = src, check = chk}) = do
  r :: Maybe (Either SomeException (Maybe String)) <- timeout (seconds tout) $ do
    er <- try src
    return $ chk <$> er
  let z = case r of
        Nothing -> Just "Timeout"
        Just (Left e) -> Just (show e)
        Just (Right mo) -> mo
  return $ fmap (\r -> title <> ": " <> r) z

-- return $ fmap (\r -> title <> ": " <> r) $ case r of
--   Nothing -> Just "Timeout"
--   Just c -> Just c
-- where
--   -- chk :: Either SomeException (Maybe B8.ByteString) -> Either B8.ByteString B8.ByteString
--   chk (Left exp) = Left (fromString . show $ exp)
--   chk (Right Nothing) = Left "Test Timeout"
--   chk (Right (Just s)) = Right s

-- Return Nothing is result is OK or Just <error-message> in case of error
-- type Check = B8.ByteString -> Maybe String

-- class AsTest t where test :: t -> IO (Maybe String)
-- instance Default where

-- data HTTPTest = HTTPTest {check :: Check, req :: Request}

-- instance Test HTTPTest where
--     test t = do
--         er <- getURL (req t)
--         return $ case er of
--             Left e -> Just (show e)
--             Right c -> check t c
