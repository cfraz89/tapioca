{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Tapioca.Internal.Types.CsvMap where

import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Kind

import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.ColSep
import Data.Tapioca.Internal.Types.HFoldable
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.GParseRecord

import GHC.Generics
import GHC.TypeLits

data CsvMapType = Bimap | EncodeMap

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped (t :: CsvMapType) r' | r' -> t where
  csvMap :: CsvMap (t :: CsvMapType) r'

type CsvDecode r (m :: Type -> Type) =
  ( GenericCsvDecode r (m r) C.NamedRecord
  , GenericCsvDecode r (m r) C.Record
  , Width m r
  )

type CsvEncode r (m :: Type -> Type) =
  ( HFoldable (m r) (r -> C.NamedRecord)
  , HFoldable (m r) (r -> C.Record)
  , HFoldable (m r) C.Header -- To List headers
  )

data CsvMap (t :: CsvMapType) r where
  CsvMap :: forall m r . (CsvDecode r m, CsvEncode r m) => m r -> CsvMap 'Bimap r
  CsvEncodeMap :: forall m r. (CsvEncode r m) => m r -> CsvMap 'EncodeMap r

class MakeCsvMap (t :: CsvMapType) r m where
  mkCsvMap :: m r -> CsvMap t r

instance (CsvDecode r m, CsvEncode r m) => MakeCsvMap 'Bimap r m where
  mkCsvMap = CsvMap

instance CsvEncode r m => MakeCsvMap 'EncodeMap r m where
  mkCsvMap = CsvEncodeMap

-- -- We can't compose codecs since we can't change the type of m (easily)
-- mappingCodec :: (r'' -> r') -> (r' -> r'') -> CsvMap 'Bimap r' -> CsvMap 'Bimap r''
-- mappingCodec enc dec (CsvMap (Codec enc' dec') m) = CsvMap (Codec (enc' . enc) (dec . dec')) m

-- -- We can't compose codecs since we can't change the type of m (easily)
-- mappingEncoder :: (r'' -> r') -> CsvMap 'EncodeMap r' -> CsvMap 'EncodeMap r''
-- mappingEncoder enc (CsvEncodeMap enc' m) = CsvEncodeMap (enc' . enc) m

infixl 5 <->
-- | Create a bidirectional mapping from name to field.
(<->) :: forall s f c r. (C.FromField c, C.ToField c) => B.ByteString -> Field s f c r -> FieldMapping s f r
name <-> field  = Bicode name field

infixl 5 <-<
-- | Create an encode-only mapping from name to field.
(<-<) :: forall f r. (C.ToField f) => B.ByteString -> EncodeField f r -> FieldMapping "" f r
name <-< ef  = Encode @f name ef

-- | Nest the record at this field into the mapping at this point.
nest :: forall s r f c. (CsvMapped 'Bimap c, Generic c) => Field s f c r -> FieldMapping s f r
nest = Nest

with :: forall s f c r m. (CsvDecode c m, CsvEncode c m) => Field s f c r -> m c -> FieldMapping s f r
with f = With f . mkCsvMap @'Bimap

-- | A mapping for a single field in our record.
-- A `CsvMap` is a chain of FieldMappings joined with `:|`
--
-- Can be created with:
--
--   * '<->' to map a single field
--   * 'nest' to nest the record at this field
data FieldMapping (s :: Symbol) f r where
  Bicode :: forall s f c r. (C.FromField c, C.ToField c) => B.ByteString -> Field s f c r -> FieldMapping s f r
  Encode :: C.ToField f => B.ByteString -> EncodeField f r -> FieldMapping "" f r
  Nest :: forall s f c r. (CsvMapped 'Bimap c, Generic c) => Field s f c r -> FieldMapping s f r
  With :: forall s f c r. Field s f c r -> CsvMap 'Bimap c -> FieldMapping s f r

-- | Match instance
type instance Match (FieldMapping s f) s' = EqSymbol s s'

-- | Class for terms which can be reduced to a FieldMapping
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f where
  selectorMapping :: t -> FieldMapping s f r

-- | Reduce instance for a single FieldMapping
instance (r~r', f~f') => Reduce (FieldMapping s f r) s r' f' where
  selectorMapping = id

-- | Reduce induction
instance (Reduce tt s r f, m1 ~ Match t1 s, m2 ~ Match t2 s, PickMatch t1 t2 m1 m2 s r, tt ~ Picked t1 t2 m1 m2 s r) => Reduce ((t1 :| t2) r) s r f where
 selectorMapping t = selectorMapping (picked @_ @_ @m1 @m2 @s t)

-- | Support for encoding
instance HFoldVal (FieldMapping s f r) (r -> C.Record) where
  hFoldVal fm = case fm of
    Bicode _ (Field field (Codec enc _)) -> V.singleton . C.toField . enc . field
    Encode _ (EncodeField e) -> V.singleton . C.toField . e
    Nest (Field field (Codec enc _)) -> toRecord (csvMap @'Bimap) . enc . field
    With (Field field (Codec enc _)) (cm :: CsvMap 'Bimap c) -> toRecord cm . enc . field

instance HFoldVal (FieldMapping s f r) (r -> C.NamedRecord) where
  hFoldVal fm = case fm of
    Bicode name (Field field (Codec enc _)) -> HM.singleton name . C.toField . enc . field
    Encode name (EncodeField e) -> HM.singleton name . C.toField . e
    Nest (Field field (Codec enc _)) -> toNamedRecord (csvMap @'Bimap) . enc . field
    With (Field field (Codec enc _)) (cm :: CsvMap 'Bimap c) -> toNamedRecord cm . enc . field

-- | Generate a header entry for this mapping
instance HFoldVal (FieldMapping s f r) C.Header where
  hFoldVal = \case
    Bicode name _ -> pure name
    Encode name _ -> pure name
    Nest (_ :: Field s f c r) -> hFoldOf (csvMap @_ @c)
    With (_ :: Field s f c r) cm -> hFoldOf cm
    where hFoldOf :: CsvMap 'Bimap c -> C.Header
          hFoldOf (CsvMap m) = foldHeader m

foldHeader :: HFoldable m C.Header => m -> C.Header
foldHeader = hFoldMap @_ @C.Header id

instance Index (FieldMapping s f r) s where
  index _ = 0

instance Width (FieldMapping s f) r where
  width = \case
    Bicode _ _ -> 1
    Encode _ _ -> 0 -- Not relevant for encoding
    Nest (_ :: Field s f c r) -> widthOf (csvMap @_ @c)
    With _ cm -> widthOf cm
    where widthOf :: CsvMap 'Bimap c -> Int
          widthOf (CsvMap mapping) = width mapping

-- Encoding
-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- Similar to cassava's headerOrder function.
header :: forall t r. CsvMap t r -> C.Header
header (CsvMap m) = foldHeader m
header (CsvEncodeMap m) = foldHeader m

-- | Encode a single record to a cassava 'Record' by ordering.
toRecord :: forall t r. CsvMap t r -> r -> C.Record
toRecord (CsvMap mapping) record = foldRecord record mapping
toRecord (CsvEncodeMap mapping) record = foldRecord record mapping

-- | Encode a single record to a cassava 'NamedRecord'.
toNamedRecord :: forall t r. CsvMap t r -> r -> C.NamedRecord
toNamedRecord (CsvMap mapping) record = foldRecord record mapping
toNamedRecord (CsvEncodeMap mapping) record = foldRecord record mapping

-- | Fold over our type-level list, converting our CsvMapped type to a cassava record
-- r - CsvMapped instance type
-- m - Type of mapping list
-- rt - Cassava record type
foldRecord :: forall m r rt. (HFoldable m (r -> rt), Semigroup rt) => r -> m -> rt
foldRecord record = hFoldMap @_ @(r -> rt) ($ record)
