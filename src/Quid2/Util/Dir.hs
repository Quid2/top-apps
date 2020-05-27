module Quid2.Util.Dir
  ( makeDir
  , makeNewDir
  )
where

import           System.Directory

makeDir :: FilePath -> IO FilePath
makeDir dir = do
  createDirectoryIfMissing False dir
  return dir

makeNewDir :: FilePath -> IO FilePath
makeNewDir dir = do
  createDirectoryIfMissing False dir
  removeDirectoryRecursive dir >> createDirectoryIfMissing False dir
  return dir
