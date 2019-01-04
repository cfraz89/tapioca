{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Tapioca.Example.Types where

import GHC.Generics
import Data.Tapioca
import Type.Reflection

data ExampleRecord = ExampleRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic, Typeable)

data SplicingRecord = SplicingRecord
  { exampleRecord :: ExampleRecord
  , other :: Int
  }
  deriving (Show, Generic)

instance CsvMapped ExampleRecord where
 csvMap = mkCsvMap
   [ "Sample Field 1" := mapCodecs asOrdinal fromOrdinal #field1
   , "Sample Field 3" := #field3
   , "Sample Field 2" := #field2
   ]

instance CsvMapped SplicingRecord where
  csvMap = mkCsvMap
    [ splice #exampleRecord
    , "Other" := #other
    ]

--------------------------------------

asOrdinal :: Int -> String
asOrdinal 0 = "Zeroth?" 
asOrdinal 1 = "First"
asOrdinal 2 = "Second"
asOrdinal 3 = "Third"
asOrdinal x = show x

fromOrdinal :: String -> Int
fromOrdinal "Zeroth?" = 0
fromOrdinal "First" = 1
fromOrdinal "Second" = 2
fromOrdinal "Third" = 3
fromOrdinal x = read x