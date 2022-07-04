{-# LANGUAGE OverloadedStrings #-}

module Test.GPG(gpgDecrypt,gpgDecryptValue) where
import qualified Data.ByteString.Char8      as B8
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.String                (IsString (fromString))
import           System.Process.Typed       (readProcess)

gpgDecryptValue :: Read a => FilePath -> IO a
gpgDecryptValue f = read . L8.unpack . head . L8.lines <$> gpgDecrypt f

-- Assumes that private key of receiver is present on host
gpgDecrypt :: FilePath  -> IO ByteString
gpgDecrypt f = do
    (exitCode, out, err) <- readProcess $ fromString $ "gpg --quiet --openpgp --decrypt " ++ f
    -- print (exitCode :: ExitCode)
    -- print (out :: ByteString)
    -- print (err :: ByteString)
    return out
