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
import Data.Tapioca.Internal.Types.Capability

import GHC.Exts
import GHC.Generics

import qualified Data.Csv as C

class CsvMapped cs r => ParseWithCsvMap (cs :: [Capability]) r t where
  parseWithCsvMap :: t -> C.Parser r

instance (CsvMapped cs r, Can 'Decode cs, Generic r) => ParseWithCsvMap cs r C.NamedRecord where
  parseWithCsvMap namedRecord = parseFrom (csvMap @_ @r)
    where parseFrom :: CsvMap cs r -> C.Parser r
          parseFrom (CsvMap m) = to <$> gParseRecord @(Rep r) @r proxy# m namedRecord
          parseFrom (CsvEncode _) = undefined -- Should never match due to type constraints

instance (CsvMapped cs r, Can 'Decode cs) => ParseWithCsvMap cs r C.Record where
  parseWithCsvMap record = parseFrom (csvMap @_ @r)
    where parseFrom :: CsvMap cs r -> C.Parser r
          parseFrom (CsvMap m) = to <$> gParseRecord @(Rep r) @r proxy# m record
          parseFrom (CsvEncode _) = undefined -- Should never match due to type constraints
