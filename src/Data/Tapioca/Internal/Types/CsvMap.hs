{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Data.Tapioca.Internal.Types.CsvMap where

import Control.Lens (view)
import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.Tapioca.Internal.Types.FieldCodec
import Data.Tapioca.Internal.Types.ColSep
import Data.Tapioca.Internal.Types.HFoldable
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.GParseRecord

import GHC.Generics
import GHC.TypeLits

data CsvMap r = forall m.
  ( GenericCsvDecode r m C.NamedRecord
  , GenericCsvDecode r m C.Record
  , HFoldable m (r -> C.NamedRecord)
  , HFoldable m (r -> V.Vector C.Field) -- For encode ToRecord
  , HFoldable m C.Header -- To List headers
  , Width m
  ) =>
  CsvMap m

infixl 3 <->
-- | This is the core type class of tapioca. Implement it in your types to support easy encoding to CSV
class CsvMapped r where
  csvMap :: CsvMap r

  (<->) :: forall s f c. (C.FromField c, C.ToField c) => B.ByteString -> FieldCodec s r f c -> FieldMapping s r f
  name <-> cdc  = Field name cdc

-- | The 'link' in a mapping chain.
data FieldMapping (s :: Symbol) r f where
  Field :: forall s r f c. (C.FromField c, C.ToField c) => B.ByteString -> FieldCodec s r f c -> FieldMapping s r f
  Nest :: forall s r f c. (CsvMapped c, Generic c) => FieldCodec s r f c -> FieldMapping s r f

-- | Match instance
type instance Match (FieldMapping s _ _) s' = EqSymbol s s'

-- | Class for terms which can be reduced to a FieldMapping
-- The goal is to reduce to FieldMapping
class Reduce t (s :: Symbol) r f where
  selectorMapping :: t -> FieldMapping s r f

-- | Reduce instance for a single FieldMapping
instance (r~r', f~f') => Reduce (FieldMapping s r f) s r' f' where
  selectorMapping = id

-- | Reduce induction
instance (Reduce tt s r f, m ~ Match t1 s, PickMatch t1 t2 m, tt ~ Picked t1 t2 m) => Reduce (t1 :| t2) s r f where
 selectorMapping t = selectorMapping (picked @_ @_ @m t)

-- | Support for encoding
instance HFoldVal (FieldMapping s r f) (r -> C.Record) where
  hFoldVal fm = case fm of
    Field _ FieldCodec{..} -> V.singleton . C.toField . view (_field . _codec)
    Nest FieldCodec{..} -> toRecord . view (_field . _codec)

instance HFoldVal (FieldMapping s r f) (r -> C.NamedRecord) where
  hFoldVal fm = case fm of
    Field name FieldCodec{..} -> HM.singleton name . C.toField . view (_field . _codec)
    Nest FieldCodec{..} -> toNamedRecord . view (_field . _codec)

-- | Generate a header entry for this mapping
instance HFoldVal (FieldMapping s r f) C.Header where
  hFoldVal (Field name _) = pure name
  hFoldVal (Nest (_ :: FieldCodec s r f c)) = hFoldOf (csvMap @c)
    where hFoldOf (CsvMap (m :: t)) = hFoldMap @_ @C.Header id m

instance Index (FieldMapping s r f) s where
  index _ = 0

instance Width (FieldMapping s r f) where
  width (Field _ _) = 1
  width (Nest (_ :: FieldCodec _ _ _ c)) = widthOf (csvMap @c)
    where widthOf (CsvMap mapping) = width mapping

-- Encoding
-- | Return a vector of all headers specified by our csv map in order. Nested maps will have their headers spliced inline.
-- | Similar to cassava's headerOrder function
header :: forall r. CsvMapped r => C.Header
header = fromCsvMap (csvMap @r)
  where fromCsvMap (CsvMap mapping) = hFoldMap @_ @C.Header id mapping

-- | Tapioca equivalent of cassava's toRecord
toRecord :: forall r. CsvMapped r => r -> C.Record
toRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap mapping) = hFoldMap @_ @(r -> C.Record) ($ record) mapping

-- | Tapioca equivalent of cassava's toNamedRecord
toNamedRecord :: forall r. CsvMapped r => r -> C.NamedRecord
toNamedRecord record = foldCsvMap (csvMap @r)
  where foldCsvMap (CsvMap mapping) = hFoldMap @_ @(r -> C.NamedRecord) ($ record) mapping
