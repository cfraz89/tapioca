{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.ByCsvMap where

import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.ParseWithCsvMap (ParseWithCsvMap(..))

import GHC.Generics
import qualified Data.Csv as C

-- | A newtype which provides instances for Cassava's To*, From*, and DefaultOrdered typeclasses
-- Can be used with DerivingVia to provide these instances to your records directly. Refer to
-- CassavaCompat example for a demonstration.
newtype ByCsvMap a = ByCsvMap { unByCsvMap :: a }

-- | Provides Cassava instances for our records wrapped in ByCsvMap.
instance CsvMapped t r => C.ToRecord (ByCsvMap r) where
  toRecord (ByCsvMap a) = toRecord @t a

instance CsvMapped t r => C.ToNamedRecord (ByCsvMap r) where
  toNamedRecord (ByCsvMap a) = toNamedRecord @t a

instance CsvMapped t r => C.DefaultOrdered (ByCsvMap r) where
  headerOrder _ = header @t @r

instance (CsvMapped t r, ParseWithCsvMap t r C.Record) => C.FromRecord (ByCsvMap r) where
  parseRecord = (ByCsvMap <$>) . parseWithCsvMap @t

instance (CsvMapped t r, Generic r, ParseWithCsvMap t r C.NamedRecord) => C.FromNamedRecord (ByCsvMap r) where
  parseNamedRecord = (ByCsvMap <$>) . parseWithCsvMap @t
