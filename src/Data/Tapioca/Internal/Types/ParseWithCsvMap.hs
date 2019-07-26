{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Types.ParseWithCsvMap where

import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.GParseRecord

import GHC.Exts
import GHC.Generics

import qualified Data.Csv as C

class ParseWithCsvMap r t where
  parseWithCsvMap :: CsvMapped r => t -> C.Parser r

instance Generic r => ParseWithCsvMap r C.NamedRecord where
  parseWithCsvMap namedRecord = parseFrom csvMap
    where parseFrom (CsvMap m :: CsvMap r) = to <$> gParseRecord @(Rep r) @r proxy# m namedRecord
          parseFrom (CsvEncodeMap _) = fail "Cannot parse into an encode-only map"

instance ParseWithCsvMap r C.Record where
  parseWithCsvMap record = parseFrom csvMap
    where parseFrom (CsvMap m :: CsvMap r) = to <$> gParseRecord @(Rep r) @r proxy# m record
          parseFrom (CsvEncodeMap _) = fail "Cannot parse into an encode-only map"
