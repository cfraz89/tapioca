{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.ColSep
import Data.Tapioca.Internal.Types.HFoldable
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.GParseRecord
import Data.Tapioca.Internal.Types.Capability

import GHC.Generics
import GHC.TypeLits

-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped (t :: [Capability]) r' | r' -> t where
  csvMap :: CsvMap t r'

type CsvDecode r m =
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  , Width m
  )

type CsvEncode r m =
  ( HFoldable m (r -> C.NamedRecord)
  , HFoldable m (r -> C.Record)
  , HFoldable m C.Header -- To List headers
  )

data CsvMap (t :: [Capability]) r where
  CsvMap :: forall m r . (CsvDecode r (m EncodeDecode r), CsvEncode r (m EncodeDecode r)) => m EncodeDecode r -> CsvMap EncodeDecode r
  CsvEncode :: forall m r. (CsvEncode r (m Encode r)) => m Encode r -> CsvMap Encode r

class MakeCsvMap (t :: [Capability]) r m where
  mkCsvMap :: m t r -> CsvMap t r

instance (CsvDecode r (m EncodeDecode r), CsvEncode r (m EncodeDecode r)) => MakeCsvMap EncodeDecode r m where
  mkCsvMap = CsvMap

instance CsvEncode r (m Encode r) => MakeCsvMap Encode r m where
  mkCsvMap = CsvEncode

-- -- We can't compose codecs since we can't change the type of m (easily)
-- mappingCodec :: (r'' -> r') -> (r' -> r'') -> CsvMap EncodeDecode r' -> CsvMap EncodeDecode r''
-- mappingCodec enc dec (CsvMap (Codec enc' dec') m) = CsvMap (Codec (enc' . enc) (dec . dec')) m

-- -- We can't compose codecs since we can't change the type of m (easily)
-- mappingEncoder :: (r'' -> r') -> CsvMap Encode r' -> CsvMap Encode r''
-- mappingEncoder enc (CsvEncode enc' m) = CsvEncode (enc' . enc) m

infixl 5 .->

class MakeFieldMapping f c t where
  (.->) :: B.ByteString -> Field s f c t r -> FieldMapping s f t r

-- | Create a bidirectional mapping from name to field.
-- | Since t can be encode, decode, or both, leave it open
instance (C.FromField c, C.ToField c) => MakeFieldMapping f c EncodeDecode where
  name .-> field'  = BicodeFM name field'

-- | Create an encode-only mapping from name to field.
instance C.ToField c => MakeFieldMapping c c Encode where
  name .-> ef  = EncodeFM name ef

-- Maybe this will work with parameterised fieldmappings
class CsvMapped t c => Nestable s f c t r where
  nest :: Field s f c t r -> FieldMapping s f t r

-- | Nest the record at this field into the mapping at this point.
instance forall s r f c. (CsvMapped EncodeDecode c, Generic c) => Nestable s f c EncodeDecode r where
  nest = Nest

instance forall s r f c. (f ~ c, CsvMapped Encode f) => Nestable s f c Encode r where
  nest = NestEncode

-- | Nest the record at this field into the mapping at this point.
-- nest :: forall s r f c. (CsvMapped EncodeDecode c, Generic c) => Field s f c r -> FieldMapping s f r
-- nest = Nest

-- nestEncode :: forall s r f. (CsvMapped Encode f) => EncodeField s f r -> FieldMapping s f r
-- nestEncode = NestEncode

class Withable m s f c (t :: [Capability]) r where
  with :: Field s f c t r -> m t c -> FieldMapping s f t r

-- | Nest the record at this field into the mapping at this point.
instance (CsvDecode c (m EncodeDecode c), CsvEncode c (m EncodeDecode c)) => Withable m s f c EncodeDecode r where
  with f = With f . mkCsvMap @EncodeDecode

instance (f ~ c, CsvEncode c (m Encode c)) => Withable m s f c Encode r where
  with f = WithEncode f . mkCsvMap

-- with :: forall s f c r m. (CsvDecode c m, CsvEncode c m) => Field s f c r -> m c -> FieldMapping s f r
-- with f = With f . mkCsvMap @EncodeDecode

-- | A mapping for a single field in our record.
-- A `CsvMap` is a chain of FieldMappings joined with `:|`
--
-- Can be created with:
--
--   * '<->' to map a single field
--   * 'nest' to nest the record at this field
data FieldMapping (s :: Symbol) f (t :: [Capability]) r where
  BicodeFM :: forall s f c t r. (C.FromField c, C.ToField c) => B.ByteString -> Field s f c EncodeDecode r -> FieldMapping s f t r -- Any t allowed for bicoding
  EncodeFM :: forall s f r. C.ToField f => B.ByteString -> Field s f f Encode r -> FieldMapping s f Encode r
  Nest :: forall s f c t r. (CsvMapped EncodeDecode c, Generic c) => Field s f c EncodeDecode r -> FieldMapping s f t r
  NestEncode :: forall s f r. CsvMapped Encode f => Field s f f Encode r -> FieldMapping s f Encode r
  With :: forall s f c t r. Field s f c EncodeDecode r -> CsvMap EncodeDecode c -> FieldMapping s f t r
  WithEncode :: forall s f r. Field s f f Encode r -> CsvMap Encode f -> FieldMapping s f Encode r

-- | Match instance
type instance Match (FieldMapping s f t r) s' = EqSymbol s s'

-- | Class for terms which can be reduced to a FieldMapping
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) f (mt :: [Capability]) r | t -> mt where
  selectorMapping :: t -> FieldMapping s f mt r

-- | Reduce instance for a single FieldMapping
instance (r~r', f~f') => Reduce (FieldMapping s f mt r) s f' mt r' where
  selectorMapping = id

-- | Reduce induction
instance (Reduce tt s f mt r, m1 ~ Match (t1 mt r) s, m2 ~ Match (t2 mt r) s, PickMatch ((t1 :| t2) mt r) m1 m2 s, tt ~ Picked ((t1 :| t2) mt r) m1 m2 s) => Reduce ((t1 :| t2) mt r) s f mt r where
 selectorMapping t = selectorMapping (picked @_ @m1 @m2 @s t)

-- | Support for encoding
instance HFoldVal (FieldMapping t s f r) (r -> C.Record) where
  hFoldVal fm = case fm of
    BicodeFM _ (Field get (Codec enc _)) -> V.singleton . C.toField . enc . get
    EncodeFM _ (EncodeField e) -> V.singleton . C.toField . e
    Nest (Field get (Codec enc _)) -> toRecord (csvMap @EncodeDecode) . enc . get
    NestEncode (EncodeField enc) -> toRecord (csvMap @Encode) . enc
    With (Field get (Codec enc _)) (cm :: CsvMap EncodeDecode c) -> toRecord cm . enc . get
    WithEncode (EncodeField enc) (cm :: CsvMap Encode c) -> toRecord cm . enc

instance HFoldVal (FieldMapping t s f r) (r -> C.NamedRecord) where
  hFoldVal fm = case fm of
    BicodeFM name (Field get (Codec enc _)) -> HM.singleton name . C.toField . enc . get
    EncodeFM name (EncodeField e) -> HM.singleton name . C.toField . e
    Nest (Field get (Codec enc _)) -> toNamedRecord (csvMap @EncodeDecode) . enc . get
    NestEncode (EncodeField enc) -> toNamedRecord (csvMap @Encode) . enc
    With (Field get (Codec enc _)) (cm :: CsvMap EncodeDecode c) -> toNamedRecord cm . enc . get
    WithEncode (EncodeField enc) (cm :: CsvMap Encode c) -> toNamedRecord cm . enc

hFoldOf :: CsvMap t c -> C.Header
hFoldOf (CsvMap m) = foldHeader m
hFoldOf (CsvEncode m) = foldHeader m

-- | Generate a header entry for this mapping
instance HFoldVal (FieldMapping s f EncodeDecode r) C.Header where
  hFoldVal = \case
    BicodeFM name _ -> pure name
    Nest (_ :: Field s f c EncodeDecode r) -> hFoldOf (csvMap @_ @c)
    With (_ :: Field s f c EncodeDecode r) cm -> hFoldOf cm

instance HFoldVal (FieldMapping s f Encode r) C.Header where
  hFoldVal = \case
    BicodeFM name _ -> pure name
    EncodeFM name _ -> pure name
    Nest (_ :: Field s f c EncodeDecode r) -> hFoldOf (csvMap @_ @c)
    NestEncode (_ :: Field s f f Encode r) -> hFoldOf (csvMap @_ @f)
    With (_ :: Field s f c EncodeDecode r) cm -> hFoldOf cm
    WithEncode (_ :: Field s f f Encode r) cm -> hFoldOf cm

foldHeader :: HFoldable m C.Header => m -> C.Header
foldHeader = hFoldMap @_ @C.Header id

instance Index (FieldMapping s f t r) s where
  index _ = 0

instance Width (FieldMapping s f EncodeDecode r) where
  width = \case
    BicodeFM _ _ -> 1
    Nest (_ :: Field s f c EncodeDecode r) -> widthOf (csvMap @_ @c)
    With _ cm -> widthOf cm
    where widthOf :: CsvMap EncodeDecode c -> Int
          widthOf (CsvMap mapping) = width mapping

-- Encoding
-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- Similar to cassava's headerOrder function.
header :: forall t r. CsvMap t r -> C.Header
header (CsvMap mapping) = foldHeader mapping
header (CsvEncode mapping) = foldHeader mapping

-- | Encode a single record to a cassava 'Record' by ordering.
toRecord :: forall t r. CsvMap t r -> r -> C.Record
toRecord (CsvMap mapping) record = foldRecord record mapping
toRecord (CsvEncode mapping) record = foldRecord record mapping

-- | Encode a single record to a cassava 'NamedRecord'.
toNamedRecord :: forall t r. CsvMap t r -> r -> C.NamedRecord
toNamedRecord (CsvMap mapping) record = foldRecord record mapping
toNamedRecord (CsvEncode mapping) record = foldRecord record mapping

-- | Fold over our type-level list, converting our CsvMapped type to a cassava record
-- r - CsvMapped instance type
-- m - Type of mapping list
-- rt - Cassava record type
foldRecord :: forall m r rt. (HFoldable m (r -> rt), Semigroup rt) => r -> m -> rt
foldRecord record = hFoldMap @_ @(r -> rt) ($ record)
