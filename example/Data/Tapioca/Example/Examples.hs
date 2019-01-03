module Data.Tapioca.Example.Examples where

import Data.Tapioca
import qualified Data.ByteString as B
import Data.Vector

import Data.Tapioca.Example.Types
import Data.Tapioca.Example.Data

------ Example usages. Try them in a repl!
encodeExampleItems :: B.ByteString
encodeExampleItems = encode WithHeader exampleItems

decodeExampleItems :: Either String (Vector ExampleRecord)
decodeExampleItems = decode WithHeader exampleCsv

decodeExampleItemsNoHeader :: Either String (Vector ExampleRecord)
decodeExampleItemsNoHeader = decode WithoutHeader exampleCsv

encodeExampleSplicingItems :: B.ByteString
encodeExampleSplicingItems = encode WithHeader exampleSplicingItems

decodeExampleSplicingItems :: Either String (Vector SplicingRecord)
decodeExampleSplicingItems = decode WithHeader exampleCsv
