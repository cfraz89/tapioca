{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.ByCsvMap where

import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.ParseWithCsvMap (parseWithCsvMap)

import GHC.Generics
import qualified Data.Csv as C

-- | A newtype which provides instances for Cassava's To*, From*, and DefaultOrdered typeclasses
newtype ByCsvMap a = ByCsvMap { unByCsvMap :: a }

-- | Provides Cassava instances for our records wrapped in ByCsvMap.
instance CsvMapped r => C.ToRecord (ByCsvMap r) where
  toRecord (ByCsvMap a) = toRecord a

instance CsvMapped r => C.ToNamedRecord (ByCsvMap r) where
  toNamedRecord (ByCsvMap a) = toNamedRecord a

instance CsvMapped r => C.DefaultOrdered (ByCsvMap r) where
  headerOrder _ = header @r

instance CsvMapped r => C.FromRecord (ByCsvMap r) where
  parseRecord = (ByCsvMap <$>) . parseWithCsvMap

instance (CsvMapped r, Generic r) => C.FromNamedRecord (ByCsvMap r) where
  parseNamedRecord = (ByCsvMap <$>) . parseWithCsvMap
