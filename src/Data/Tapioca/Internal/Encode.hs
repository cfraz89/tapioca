{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tapioca.Internal.Encode
  ( toRecord
  , toNamedRecord
  ) where

import Data.Tapioca.Internal.Types.Codec
import Data.Tapioca.Internal.Types.Separator
import Data.Tapioca.Internal.Types.Mapping

import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V


-- | Tapioca equivalent of cassava's toRecord
toRecord :: forall r. (CsvMapped r, HFoldable r B.Bytestring) => r -> C.Record
toRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap m) = hFoldMap (toFields m
        toFields (Field _ fm) = V.singleton . C.toField $ encoder fm record
        toFields (Splice fm) = toRecord $ encoder fm record

-- | Tapioca equivalent of cassava's toNamedRecord
toNamedRecord :: CsvMapped r => r -> C.NamedRecord
toNamedRecord = undefined
-- toNamedRecord record = foldMap namedRecord (unCsvMap csvMap)
--     where namedRecord (name := fm) = HM.singleton name (C.toField $ encoder fm record)
--           namedRecord (Splice fm) = toNamedRecord $ encoder fm record
