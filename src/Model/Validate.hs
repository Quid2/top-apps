{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Model.Validate where

import           Data.Word
import           ZM
import qualified ZM.Type.String as Z

data Validate a =
  Validate a
  deriving (NFData, Eq, Ord, Show, Generic, Flat)

instance (Model a) => Model (Validate a)

data Issues e w =
  Issues
    { errors   :: [e]
    , warnings :: [w]
    }
  deriving (NFData, Eq, Ord, Show, Generic)

instance (Model a, Model b) => Model (Issues a b)

instance {-# OVERLAPPING #-} (Flat a, Flat b) => Flat (Issues a b)

-- MAYBE Text
data SourceCode l =
  SourceCode l Z.String
  deriving (NFData, Eq, Ord, Show, Generic, Flat)

instance (Model a) => Model (SourceCode a)

data ZM =
  ZM
  deriving (NFData, Eq, Ord, Show, Generic, Flat, Model)

data Note n o =
  Note
    { annotation :: n
    , object     :: o
    }
  deriving (NFData, Eq, Ord, Show, Generic, Flat)

instance (Model a, Model b) => Model (Note a b)

data Range =
  Range
    { start :: Position
    , end   :: Position
    }
  deriving (NFData, Eq, Ord, Show, Generic, Flat, Model)

data Position =
  Position
    { row, column :: Word32
    }
  deriving (NFData, Eq, Ord, Show, Generic, Flat, Model)
