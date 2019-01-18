{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

--Remove later
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
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
  , Header(..)
  , FieldMapping ((:=))
  , (:|)(..)
  , encode
  , decode
  , toRecord
  , toNamedRecord
  , parseRecord
  , header
  , mkCsvMap
  ) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.Generics
import GHC.Exts
import qualified Data.Csv as C

import Data.Tapioca.Internal.ByCsvMap
import Data.Tapioca.Internal.Common
import Data.Tapioca.Internal.Decode
import Data.Tapioca.Internal.Decode.Generic
import Data.Tapioca.Internal.Encode
import Data.Tapioca.Internal.Types

import qualified Data.Attoparsec.ByteString.Lazy as AB
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString as B
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
encode :: forall r. CsvMapped r => Header -> [r] -> BL.ByteString
encode withHeader items = BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (header @r) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = foldMap (CB.encodeRecord . ByCsvMap) items

-- | Decode a CSV String. If there is an error parsion, error message is returned on the left
decode :: forall r. CsvMapped r => Header -> BL.ByteString -> Either String (V.Vector r)
decode useHeader csv = C.runParser $ do
   (mbHdr, record) <- toParser $ parseCsv @r proxy# csv useHeader
   traverse (parseRecord @r (Record mbHdr)) record

-- Parse the required data from the csv file
parseCsv :: CsvMapped r => Proxy# r -> BL.ByteString -> Header -> Either String (Maybe (V.Vector B.ByteString), C.Csv)
parseCsv _ csv useHeader = AB.eitherResult . flip AB.parse csv $ do
  hdr <- case useHeader of
    WithHeader -> Just <$> (CP.header . fromIntegral . fromEnum) ','
    WithoutHeader -> pure Nothing
  records <- CP.csv C.defaultDecodeOptions
  pure (hdr, records)

data Dummy = Dummy { dt :: Int } deriving Generic

type Fm1 = FieldMapping "dt" Dummy Int Int Int


instance CsvMapped Dummy where
  csvMap = CsvMap $ "Hi I'm" := #dt


decodeTest :: C.Parser Dummy
decodeTest = to <$> gParseRecord @_ @Dummy proxy# (csvMap @Dummy) mempty
