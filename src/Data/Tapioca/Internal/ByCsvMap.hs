{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}

module Data.Tapioca.Internal.ByCsvMap where

import Data.Tapioca.Internal.Types.Capability
import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.ParseWithCsvMap (ParseWithCsvMap(..))

import GHC.Generics
import qualified Data.Csv as C

-- | A newtype which provides instances for Cassava's To*, From*, and DefaultOrdered typeclasses
-- Can be used with DerivingVia to provide these instances to your records directly. Refer to
-- CassavaCompat example for a demonstration.
newtype ByCsvMap a = ByCsvMap { unByCsvMap :: a }

-- | Provides Cassava instances for our records wrapped in ByCsvMap.
instance (CsvMapped cs r, Can 'Encode cs) => C.ToRecord (ByCsvMap r) where
  toRecord (ByCsvMap a) = toRecord (csvMap @cs) a

instance (CsvMapped cs r, Can 'Encode cs) => C.ToNamedRecord (ByCsvMap r) where
  toNamedRecord (ByCsvMap a) = toNamedRecord (csvMap @cs) a

instance (CsvMapped cs r, Can 'Encode cs) => C.DefaultOrdered (ByCsvMap r) where
  headerOrder _ = header (csvMap @cs @r)

instance (ParseWithCsvMap cs r C.Record, Can 'Decode cs) => C.FromRecord (ByCsvMap r) where
  parseRecord = (ByCsvMap <$>) . parseWithCsvMap @cs

instance (Generic r, ParseWithCsvMap cs r C.NamedRecord, Can 'Decode cs) => C.FromNamedRecord (ByCsvMap r) where
  parseNamedRecord = (ByCsvMap <$>) . parseWithCsvMap @cs
