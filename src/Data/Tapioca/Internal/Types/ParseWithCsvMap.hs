{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Tapioca.Internal.Types.ParseWithCsvMap where

import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.GParseRecord
import Data.Tapioca.Internal.Types.CsvMapType

import GHC.Exts
import GHC.Generics

import qualified Data.Csv as C

class ParseWithCsvMap (m :: CsvMapType) r t where
  parseWithCsvMap :: CsvMapped m r => t -> C.Parser r

instance Generic r => ParseWithCsvMap 'Both r C.NamedRecord where
  parseWithCsvMap namedRecord = parseFrom (csvMap @_ @r)
    where parseFrom :: CsvMap 'Both r -> C.Parser r
          parseFrom (CsvMap m) = to <$> gParseRecord @(Rep r) @r proxy# m namedRecord

instance ParseWithCsvMap 'Both r C.Record where
  parseWithCsvMap record = parseFrom (csvMap @_ @r)
    where parseFrom :: CsvMap 'Both r -> C.Parser r
          parseFrom (CsvMap m) = to <$> gParseRecord @(Rep r) @r proxy# m record
