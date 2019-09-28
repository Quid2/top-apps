module Main where

import           Data.List            (isSuffixOf)
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
  files <-
    find
      always
      ((extension ==? ".hs") &&?
       exceptFiles ["Network/Wai/Middleware/StaticHost.hs"])
      "src"
  -- files2 <- find always ((extension ==? ".hs") &&? exceptFiles []) "app"
      -- print files
  doctest $ "-XCPP" : files -- ++ files2

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  let excludes = liftOp (\fp modules -> not $ any (`isSuffixOf` fp) modules)
   in filePath `excludes` mdls
          -- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
