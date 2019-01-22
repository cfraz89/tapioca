{-# LANGUAGE GADTs #-}

module Data.Tapioca.Internal.Types.Indexing where

import qualified Data.Csv as C
-- | When encoding, whether or not to write the header row.\n
-- When decoding, whether or not the csv being decoded contains a header row.\n
-- if decoding WithoutHeader, tapioca will map the order of fields in the csv
-- to the order that fields are specified in the csvMap.

data DecodeIndexing r t where
  DecodeNamed :: DecodeIndexing r C.NamedRecord -- assumes presence of header
  DecodeOrdered :: DecodeIndexing r C.Record
