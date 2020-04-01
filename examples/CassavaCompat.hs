{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This Example demonstrates how DerivingVia can be combined with
-- the ByCsvMap type to provide implementation of Cassava's encoding typeclasses.
-- This then allows you to use Cassava's encode and decode functions
-- at will.
module Data.Tapioca.Examples.CassavaCompat where

import Data.Tapioca

import qualified Data.Csv as Csv
import GHC.Generics
import Text.Pretty.Simple

data BasicRecord = BasicRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)
  deriving
    ( Csv.FromNamedRecord
    , Csv.ToNamedRecord
    , Csv.DefaultOrdered
    , Csv.ToRecord
    , Csv.FromRecord
    ) via ByCsvMap BasicRecord

instance CsvMapped EncodeDecode BasicRecord where
 csvMap = mkCsvMap
    $ "Sample Field 1" .-> #field1
   :| "Sample Field 3" .-> #field3
   :| "Sample Field 2" .-> #field2


main :: IO ()
main = do
  pPrint $ Csv.encodeDefaultOrderedByName [BasicRecord 1 "Test" (Just 3)]
  pPrint $ Csv.encodeByName (header @BasicRecord csvMap) [BasicRecord 1 "Test" (Just 3)]
  pPrint $ Csv.encode [BasicRecord 1 "Test" (Just 3)]
  pPrint $ Csv.decode @BasicRecord Csv.NoHeader "1,3,Test"
  pPrint $ Csv.decodeByName @BasicRecord "Sample Field 1,Sample Field 2,Sample Field 3\r\n1,Test,3"
