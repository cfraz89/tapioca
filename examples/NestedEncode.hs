{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Demonstration of how records are nested on encoding
module Data.Tapioca.Examples.NestedEncode where

import GHC.Generics
import Text.Pretty.Simple
import Data.Tapioca

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)

instance CsvMapped 'Bimap BasicRecord where
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

instance CsvMapped 'Bimap NestingRecord where
  csvMap = mkCsvMap
     $ nest @'Bimap #nested
    :| "Other" <-> #someOtherData
    :| "Data" <-> #someData

main :: IO ()
main = pPrint $ encode HasHeader
  [ NestingRecord "Some data" (BasicRecord 1 "This is field 2" (Just 3)) 4
  , NestingRecord "Some more data" (BasicRecord 2 "This is field 2 agagin" (Just 4)) 5
  ]
