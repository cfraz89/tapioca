module Data.Tapioca.Example.Examples where

import Data.Tapioca
import qualified Data.ByteString as B

import Data.Tapioca.Example.Types
import Data.Tapioca.Example.Data

------ Example usages. Try them in a repl!
encodeExampleItems :: B.ByteString
encodeExampleItems = encode WithHeader exampleItems

decodeExampleItems :: [ExampleRecord]
decodeExampleItems = decode WithHeader exampleCsv

decodeExampleItemsNoHeader :: [ExampleRecord]
decodeExampleItemsNoHeader = decode WithoutHeader exampleCsv

encodeExampleSplicingItems :: B.ByteString
encodeExampleSplicingItems = encode WithHeader exampleSplicingItems

decodeExampleSplicingItems :: [SplicingRecord]
decodeExampleSplicingItems = decode WithHeader exampleCsv

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
