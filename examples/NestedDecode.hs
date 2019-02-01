{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Tapioca.Examples.BasicEncode where

import GHC.Generics
import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

instance CsvMapped BasicRecord where
 csvMap = CsvMap
    $ "Sample Field 1" <-> #field1
   :| "Sample Field 3" <-> #field3
   :| "Sample Field 2" <-> #field2

data NestingRecord = NestingRecord
  { someData :: String
  , nested :: BasicRecord
  , someOtherData :: Int
  }
  deriving (Show, Generic)

instance CsvMapped NestingRecord where
  csvMap = CsvMap
    $ Nest #nested
    :| "Other" <-> #someOtherData
    :| "Data" <-> #someData

main :: IO ()
main = do
  let exampleNestedCsv = "Sample Field 1,Sample Field 3,Sample Field 2,Other,Data\r\n"
                        <> "76,general data,,4"

  print $ decode @NestingRecord (DecodeOrdered HasHeader) exampleNestedCsv
