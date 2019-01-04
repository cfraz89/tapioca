{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Tapioca

import Data.Tapioca.Example.Types

main :: IO ()
main = do
  ----------- Basic Encode Example
  let exampleRecords = 
        [ ExampleRecord 1 "This is field 2" (Just 3)
        , ExampleRecord 2 "This is field 2 again" (Just 6)
        ]

  putStrLn "Encode Example Records ----------------"
  print $ encode WithHeader exampleRecords
  putStrLn mempty

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
  print $ encode WithHeader exampleSplicingRecords
  putStrLn mempty

  ----------- Decode spliced records
  let exampleSplicedCsv = "Sample Field 1,Sample Field 3,Sample Field 2,Other\r\n"
                        <> "First,3,This is field 2,4"
  putStrLn "Decode Example Spliced CSV ------------"
  print $ decode @SplicingRecord WithHeader exampleSplicedCsv
  putStrLn mempty