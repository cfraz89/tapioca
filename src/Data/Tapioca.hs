{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

--Remove later
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
    -- Each mapping can be mapped in either direction using the `Data.Profunctor` instance functions 'Data.ProFunctor.lmap' to map encoding,
    -- 'Data.ProFunctor.rmap' to map decoding, or 'Data.ProFunctor.dimap' for both. Refer to examples to see this in practice.
    -- $example-class

    -- | == Encoding and decoding
    -- The 'encode' and 'decode' functions will infer our 'CsvMapped' type and perform the mapping.
    -- Type applications may be needed on 'decode' depending on the use context.
    -- $example-coding

    CsvMap(..)
  , CsvMapped(..)
  , ByCsvMap(..)
  , DecodeIndexing(..)
  , FieldMapping(..)
  , (:|)(..)
  , encode
  , decode
  , header
  , C.HasHeader(..)
  ) where

import GHC.Generics

import Data.Tapioca.Internal.ByCsvMap
import Data.Tapioca.Internal.Common
import Data.Tapioca.Internal.Encode
import Data.Tapioca.Internal.Types.Mapping

import qualified Data.Attoparsec.ByteString.Lazy as AB
import qualified Data.Binary.Builder as BB

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Csv.Builder as CB
import qualified Data.Csv.Parser as CP
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

-- | Construct a CsvMap from a list of mappings
--mkCsvMap :: fm -> CsvMap r fm
--mkCsvMap = CsvMap

-- | Encode a list of items using our mapping
encode :: forall r. CsvMapped r => C.HasHeader -> [r] -> BL.ByteString
encode hasHeader items = BB.toLazyByteString $
  hdr <> mconcat (CB.encodeRecord . toRecord <$> items)
  where hdr = case hasHeader of
          C.HasHeader -> CB.encodeHeader (header @r)
          C.NoHeader -> mempty

-- | Decode a CSV String. If there is an error parsion, error message is returned on the left
decode :: forall r t. (CsvMapped r, Generic r) => DecodeIndexing r t -> BL.ByteString -> Either String (V.Vector r)
decode indexing csv = C.runParser $ do
   records <- parseCsv indexing csv
   let parse = case indexing of
         DecodeNamed -> parseWithCsvMap
         DecodeOrdered _ -> parseWithCsvMap
   traverse parse records

-- Parse the required data from the csv file
parseCsv :: forall r t. CsvMapped r => DecodeIndexing r t -> BL.ByteString -> C.Parser (V.Vector t)
parseCsv indexing csv = toParser . AB.eitherResult . flip AB.parse csv $ case indexing of
    DecodeNamed -> snd <$> CP.csvWithHeader C.defaultDecodeOptions
    DecodeOrdered C.HasHeader -> CP.header (toEnum $ fromEnum ',') >> CP.csv C.defaultDecodeOptions
    DecodeOrdered C.NoHeader -> CP.csv C.defaultDecodeOptions

data Dummy = Dummy { dt :: Int, dt2 :: String} deriving (Generic, Show)
data SpliceDummy = SpliceDummy { sd1 :: String, sd2 :: Int} deriving (Generic, Show)

instance CsvMapped Dummy where
  csvMap = CsvMap $ "Column 1" <-> #dt :| "Column 2" <-> #dt2
