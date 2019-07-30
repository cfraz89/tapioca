{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Functions needed in both encoding and decoding
module Data.Tapioca.Internal.Common
  ( (?!)
  , toParser
  , bsVectorString
  , DecodeIndexing(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Error.Util
import qualified Data.Csv as C
import Data.Tapioca.Internal.Types.ParseWithCsvMap
import Data.Tapioca.Internal.Types.CsvMapType

infixl 1 ?!
(?!) :: Maybe a -> b -> Either b a
(?!) = flip note

toParser :: Either String a -> C.Parser a
toParser (Left e) = fail e
toParser (Right a) = pure a

bsVectorString :: [B.ByteString] -> String
bsVectorString = BC.unpack . BC.intercalate ","

-- | The method via which to attempt decoding of the record
data DecodeIndexing r t where
  -- | Use the csv's header row to match against our field mappings. This is the primary use case.
  DecodeNamed :: ParseWithCsvMap 'Both r C.NamedRecord => DecodeIndexing r C.NamedRecord
  -- | Attempt to read the csv in the same order as our mapping has been defined.
  -- If HasHeader is set, the first row (header row) will be skipped.
  DecodeOrdered :: ParseWithCsvMap 'Both r C.Record => C.HasHeader -> DecodeIndexing r C.Record
