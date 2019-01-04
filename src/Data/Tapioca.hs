{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module builds on <http://hackage.haskell.org/package/cassava> to provide support for simpler mapping of records to and from CSV.
-- 
-- This is primarily achieved by use of modern GHC features such as HasField and OverloadedLabels.

module Data.Tapioca
  (
    -- = Example

    -- | == Defining a record
    -- First, we define a record with which we want to map to and from our csv data
    -- $example-record

    -- | == Declaring a 'CsvMapped' instance
    -- The class provides a 'CsvMap', which is a list of either:
    --
    --   * A bidirectional mapping from header to field selector, or
    --   * The field selector of a record (also implementing 'CsvMapped' to nest
    --
    -- Each mapping can be mapped in either direction using 'mapEncode', 'mapDecode', or 'mapCodec' for both.
    -- $example-class

    -- | == Encoding and decoding
    -- The 'encode' and 'decode' functions will infer our 'CsvMapped' type and perform the mapping.
    -- Type applications may be needed on 'decode' depending on the use context.
    -- $example-coding

    CsvMap(..)
  , CsvMapped(..)
  , CsvRecord(..)
  , Header(..)
  , SelectorMapping ((:=))
  , encode
  , decode
  , header
  , mkCsvMap
  , mapEncode
  , mapDecode
  , mapCodec
  ) where

import Data.Tapioca.Internal.Decode
import Data.Tapioca.Internal.Encode
import Data.Tapioca.Types

import qualified Data.Vector as V

-- $example-record
-- @
-- data TestItem = TestItem
--  { field1 :: Int
--  , field2 :: SomeItem
--  , field3 :: String
--  } deriving Generic
-- @

-- $example-class
-- @
-- instance 'CsvMapped' TestItem where
--  'csvMap' = 'mkCsvMap'
--    [ "Field 1" ':=' #field1
--    , #field2
--    , "Field 3" ':=' #field3
--    ]
--
-- instance 'CsvMapped' SomeItem where ...
-- @

-- $example-coding 
-- To encode to csv:
--
-- @
-- 'encode' 'WithHeader' testItems
-- @
--
-- To decode from csv:
--
-- @
-- 'decode' @TestItem 'WithHeader' csvByteString
-- @

-- | Map from the encoding type of the field to a new type
mapEncode :: (e -> x) -> FieldMapping r f e d -> FieldMapping r f x d
mapEncode f fm = fm { encoder = f . encoder fm }

-- | Map from a new decoding type of the field to the existing one
mapDecode :: (x -> d) -> FieldMapping r f e d -> FieldMapping r f e x
mapDecode f fm = fm { decoder = decoder fm . f }

-- | Map both encode and decode together
mapCodec :: (e -> x) -> (y -> d) -> FieldMapping r f e d -> FieldMapping r f x y
mapCodec enc dec = mapEncode enc . mapDecode dec

-- | Construct a CsvMap from a list of mappings
mkCsvMap :: [SelectorMapping r] -> CsvMap r
mkCsvMap = CsvMap . V.fromList