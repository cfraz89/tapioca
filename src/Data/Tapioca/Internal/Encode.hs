{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Encode
  ( encode
  , header
  , CsvRecord(..)
  ) where 

import Data.Tapioca.Types 
  ( Header(..)
  , CsvMap(..)
  , CsvMapped(..)
  , FieldMapping(..)
  , SelectorMapping(..)
  )


import qualified Data.Binary.Builder as BB
import qualified Data.Csv as C
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv.Builder as CB

-- | A newtype which provides instances for Cassava's ToRecord classes
newtype CsvRecord a = CsvRecord a

instance CsvMapped r => C.ToRecord (CsvRecord r) where
  toRecord (CsvRecord a) = V.concatMap toFields (unCsvMap csvMap)
    where toFields (_ := fm) = pure . C.toField . encoder fm $ a
          toFields (Splice fm) = C.toRecord . CsvRecord $ encoder fm a

instance CsvMapped r => C.ToNamedRecord (CsvRecord r) where
  toNamedRecord (CsvRecord a) = V.foldr' (\(name := fm) -> HM.insert name (C.toField . encoder fm $ a)) HM.empty (unCsvMap csvMap)

instance CsvMapped r => C.DefaultOrdered (CsvRecord r) where
  headerOrder _ = header @r

-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
header :: forall r. CsvMapped r => V.Vector B.ByteString
header = V.concatMap names $ unCsvMap (csvMap @r)
  where names (name := _) = pure name
        names (Splice (_ :: FieldMapping r f e d)) = header @f

-- | Encode a list of items using our mapping
encode :: forall r. CsvMapped r => Header -> [r] -> B.ByteString
encode withHeader items = BL.toStrict . BB.toLazyByteString $ case withHeader of
  WithHeader -> CB.encodeHeader (header @r) <> recordItems
  WithoutHeader -> recordItems
  where recordItems = foldr ((<>) . CB.encodeRecord . CsvRecord) mempty items