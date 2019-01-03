module Data.Tapioca.Example.Data where

import qualified Data.ByteString as B

import Data.Tapioca.Example.Types

exampleCsv :: B.ByteString
exampleCsv = "Sample Field 1,Sample Field 2,Sample Field 3\n9,testField,9"

exampleSplicedCsv :: B.ByteString
exampleSplicedCsv = "Sample Field 1,Sample Field 3,Sample Field 2,Other\r\nFirst,3,This is field 2,4\r\n"

exampleCsvNoHeader :: B.ByteString
exampleCsvNoHeader = "1,8,testField2\n42,10,sample data"

exampleItems :: [ExampleRecord]
exampleItems = 
  [ ExampleRecord 1 "This is field 2" (Just 3)
  , ExampleRecord 2 "This is field 2 again" (Just 6)
  ]

exampleSplicingItems :: [SplicingRecord]
exampleSplicingItems = 
  [ SplicingRecord (ExampleRecord 1 "This is field 2" (Just 3)) 4
  ]
