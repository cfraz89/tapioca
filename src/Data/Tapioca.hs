{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | This module builds on <http://hackage.haskell.org/package/cassava> to provide support for simpler mapping of records to and from CSV.
--
-- This is primarily achieved by use of modern GHC features such as HasField and OverloadedLabels.
--
-- Mappings created in Tapioca are type-safe - all fields must be accounted for when creating a mapping.

module Data.Tapioca
  (
    -- | = Usage
    -- You will need the following language extensions to use Tapioca:
    --
    --   * OverloadedStrings
    --   * OverloadedLabels
    --   * DeriveGeneric (for bidirectional mapping)

    -- | == Defining a record
    -- First, we define a record with which we want to map to and from our csv data. Generic is only needed if we intend to map the record bidirectionally.
    -- $example-record

    -- | == Declaring a 'CsvMapped' instance
    -- The class provides a 'CsvMap', which is a list of either:
    --
    --   * A bidirectional mapping from header to field selector, or
    --   * A nesting of a CsvMapped record
    --
    -- The encoding for each field selector can be extended beyond the ToField and FromField instances by providing an `Control.Lens.Iso.Iso`.

    -- | === Basic mapping
    -- $example-basic-class

    -- | === Mapping a field
    -- $example-iso-class

    -- | == Encoding and decoding
    -- The 'encode' and 'decode' functions will infer our 'CsvMapped' type and perform the mapping.
    -- Type applications may be needed on 'decode' depending on the use context.
    -- $example-coding

    CsvMap(..)
  , CsvMapped(..)
  , ByCsvMap(..)
  , DecodeIndexing(..)
  , FieldMapping
  , Field
  , EncodeField
  , Codec(..)
  , (:|)(..)
  , (<->)
  , (<-<)
  , nest
  , encode
  , decode
  , header
  , codec
  , encoder
  , encodeField
  , toRecord
  , toNamedRecord
  , mkCsvMap 
  , mkCsvEncodeMap
  , mappingCodec
  , mappingEncoder
  , C.HasHeader(..)
  , CsvMapType(..)
  ) where

import GHC.Generics

import Data.Tapioca.Internal.ByCsvMap
import Data.Tapioca.Internal.Types.ColSep
import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Common
import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.ParseWithCsvMap
import Data.Tapioca.Internal.Types.ParseRecord()

import qualified Data.Attoparsec.ByteString.Lazy as AB
import qualified Data.Binary.Builder as BB

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Builder as CB
import qualified Data.Csv.Parser as CP
import Data.Semigroup ((<>))
import qualified Data.Vector as V

-- $example-record
-- @
-- data TestItem = TestItem
--  { field1 :: Int
--  , field2 :: SomeItem
--  , field3 :: String
--  } deriving Generic
-- @

-- $example-basic-class
-- @
-- instance 'CsvMapped' TestItem where
--  'csvMap' = 'CsvMap'
--  $ "Field 1" '<->' #field1
-- ':|' 'nest' #field2
-- ':|' "Field 3" '<->' #field3
-- @

-- $example-iso-class
-- @
-- instance 'CsvMapped' TestItem where
--  'csvMap' = 'CsvMap'
--  $ "Field 1" '<->' #field1 '<:>' iso (+1) (-1)
-- ':|' 'nest' #field2
-- ':|' "Field 3" '<->' #field3
-- @

-- $example-coding
-- To encode to csv:
--
-- @
-- 'encode' 'HasHeader' testItems
-- @
--
-- To decode from csv:
--
-- @
-- 'decode' @TestItem 'DecodeNamed' csvByteString
-- @


-- | Encode a list of items using our mapping
encode :: forall (t :: CsvMapType) r. CsvMapped t r => CsvMap t r -> C.HasHeader -> [r] -> BL.ByteString
encode _ hasHeader items = BB.toLazyByteString $
  hdr <> mconcat (CB.encodeRecord . toRecord @t <$> items)
  where hdr = case hasHeader of
          C.HasHeader -> CB.encodeHeader (header @t @r)
          C.NoHeader -> mempty

-- | Decode a CSV String. If there is an error parsion, error message is returned on the left
decode :: forall r t. (CsvMapped 'Bimap r, Generic r) => CsvMap 'Bimap r -> DecodeIndexing r t -> BL.ByteString -> Either String (V.Vector r)
decode csm indexing csv = C.runParser $ do
   records <- parseCsv csm indexing csv
   let parse = case indexing of
         DecodeNamed -> parseWithCsvMap @'Bimap
         DecodeOrdered _ -> parseWithCsvMap @'Bimap
   traverse parse records

-- Parse the required data from the csv file
parseCsv :: forall m r t. CsvMapped m r => CsvMap m r -> DecodeIndexing r t -> BL.ByteString -> C.Parser (V.Vector t)
parseCsv _ indexing csv = toParser . AB.eitherResult . flip AB.parse csv $ case indexing of
    DecodeNamed -> snd <$> CP.csvWithHeader C.defaultDecodeOptions
    DecodeOrdered C.HasHeader -> CP.header (toEnum $ fromEnum ',') >> CP.csv C.defaultDecodeOptions
    DecodeOrdered C.NoHeader -> CP.csv C.defaultDecodeOptions
