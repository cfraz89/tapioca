module Data.Tapioca.Example.Types where

import GHC.Generics
import Data.Tapioca

data ExampleRecord = ExampleRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

data SplicingRecord = SplicingRecord
  { exampleRecord :: ExampleRecord
  , other :: Int
  }
  deriving (Show, Generic)

instance CsvMapped ExampleRecord where
 csvMap = mkCsvMap
   [ "Sample Field 1" := codec toOrdinal fromOrdinal #field1
   , "Sample Field 3" := #field3
   , "Sample Field 2" := #field2
   ]

instance CsvMapped SplicingRecord where
  csvMap = mkCsvMap
    [ Splice #testRecord
    , "Other" := #other
    ]
