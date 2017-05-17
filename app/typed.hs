import           Control.Exception
import qualified Data.Map               as M
import           Data.Monoid
-- import           Data.Pattern.ToHaskell
import           Network.Top          hiding (Config, info, (<>))
import           Options.Applicative

data Config = Config {refs::[AbsRef],srcDir :: FilePath}

generate cfg = do
  eenv <- knownTypes def
  case eenv of
    Left e    -> throwIO $ unwords ["Failed to retrieve list of known types:",e]
    Right env -> writeModules (srcDir cfg) . M.fromList . filter (\kv -> fst kv `elem` (refs cfg)) $ env

  print "DONE"
  -- print $ refs cfg
  -- writeModules (srcDir cfg)

parser :: Parser Config
parser = Config <$> refs <*> srcDir
  where
    refs :: Parser [AbsRef]
    refs = (map unPrettyRef <$>) $ many $ argument str $
      metavar "reference..."
      <> help "Compact references to types (example: H728233)"

    srcDir =  strOption
      ( long "srcDir"
        <> help "Source directory where generated files should be written (in the Model subdirectory)" )

main :: IO ()
main = execParser opts >>= generate
  where
     opts = info (helper <*> parser)
       ( fullDesc
         <> progDesc "Generate Haskell types from absolute types"
         <> header "typed" )

