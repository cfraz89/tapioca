{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

import Data.Tapioca

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Profunctor

-- Only needed for the demos of exporting Cassava typeclasses
import qualified Data.Csv as C

data ExampleRecord = ExampleRecord
  { field1 :: Int
  , field2 :: String
  , field3 :: Maybe Int
  }
  deriving (Show, Generic)
  deriving C.FromNamedRecord via ByCsvMap ExampleRecord

data SplicingRecord = SplicingRecord
  { exampleRecord :: ExampleRecord
  , other :: Int
  }
  deriving (Show, Generic)
  deriving C.FromNamedRecord via ByCsvMap SplicingRecord

instance CsvMapped ExampleRecord where
 csvMap = mkCsvMap
   [ "Sample Field 1" := dimap fromOrdinal asOrdinal #field1
   , "Sample Field 3" := #field3
   , "Sample Field 2" := #field2
   ]
   where asOrdinal = \case
           0 -> "Zeroth?" 
           1 -> "First"
           2 -> "Second"
           3 -> "Third"
           x -> show x

         fromOrdinal = \case
           "Zeroth?" -> 0
           "First"   -> 1
           "Second"  -> 2
           "Third"   -> 3
           x         -> read x

instance CsvMapped SplicingRecord where
  csvMap = mkCsvMap
    [ #exampleRecord
    , "Other" := #other
    ]

main :: IO ()
main = do
  putStrLn mempty
  ----------- Basic Encode Example
  let exampleRecords = 
        [ ExampleRecord 1 "This is field 2" (Just 3)
        , ExampleRecord 2 "This is field 2 again" (Just 6)
        ]

  putStrLn "Encode Example Records ----------------"
  putStrLn . BL.unpack $ encode WithHeader exampleRecords

  ------------ Basic Decode Example
  let exampleCsv = "Sample Field 1,Sample Field 2,Sample Field 3\r\n" 
                <> "First,testField,9"

  putStrLn "Decode Example CSV --------------------"
  print $ decode @ExampleRecord WithHeader exampleCsv
  putStrLn mempty

  ------------ Encode when no header
  let exampleCsvNoHeader = "First,8,testField2\r\n"
                         <> "42,10,sample data"

  putStrLn "Decode Example CSV without header -----"
  print $ decode @ExampleRecord WithoutHeader exampleCsvNoHeader
  putStrLn mempty

  ----------- Encode spliced records
  let exampleSplicingRecords = 
        [ SplicingRecord (ExampleRecord 1 "This is field 2" (Just 3)) 4
        , SplicingRecord (ExampleRecord 2 "This is field 2 agagin" (Just 4)) 5
        ]

  putStrLn "Encode Example Splicing Records--------"
  putStrLn . BL.unpack $ encode WithHeader exampleSplicingRecords

  ----------- Decode spliced records
  let exampleSplicedCsv = "Sample Field 1,Sample Field 3,Sample Field 2,Other\r\n"
                        <> "First,3,This is field 2,4"
  putStrLn "Decode Example Spliced CSV ------------"
  print $ decode @SplicingRecord WithHeader exampleSplicedCsv
  putStrLn mempty

  ----------- Cassava compatibility: FromNamedRecord
  putStrLn "Decode Example Spliced CSV with Cassava decodeByName ------------"
  print $ C.decodeByName @SplicingRecord exampleSplicedCsv
  putStrLn mempty

