{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Tapioca.Internal.Types.ParseRecord where

import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.GParseRecord
import Data.Tapioca.Internal.Common (bsVectorString)
import Data.Tapioca.Internal.Types.Capability

import GHC.Exts
import GHC.Generics
import GHC.TypeLits

import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Semigroup ((<>))
import Data.String.Conv

--Also should be Decode, as well as Both, once it exists
instance (Can 'Decode cs, Reduce t s f cs r, KnownSymbol s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @f @cs @r fieldMapping of
            BicodeFM name (Field _ _codec) -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> toS name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup (toS name) namedRecord
                    decode :: C.Field -> C.Parser f
                    decode = (_decode _codec <$>) . C.parseField
            Nest (Field _ _codec :: Field s f c EncodeDecode r) -> parseNest (csvMap @_ @c) (_decode _codec)
            With (Field _ _codec :: Field s f c EncodeDecode r) cm -> parseNest cm (_decode _codec)
            EncodeFM _ _ -> notDecodeError
            NestEncode _ -> notDecodeError
            WithEncode _ _ -> notDecodeError
          parseNest :: forall c cs'. CsvMap cs' c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . to <$> gParseRecord @_ @c proxy# cm namedRecord
          parseNest (CsvEncode _) _ = notDecodeError
          selector = symbolVal @s undefined
          notDecodeError :: C.Parser f
          notDecodeError = error $ "Field " <> selector <> " is not a decode field"

instance (Can 'Decode cs, Reduce t s f cs r, Index t s, KnownSymbol s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @f @cs @r fieldMapping of
            BicodeFM _ (Field _ _codec) -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    decode = (_decode _codec <$>) . C.parseField
                    val = record V.!? idx
            Nest (Field _ _codec :: Field s f c EncodeDecode r) -> parseNest (csvMap @_ @c) (_decode _codec)
            With (Field _ _codec :: Field s f c EncodeDecode r) cm -> parseNest cm (_decode _codec)
            EncodeFM _ _ -> notDecodeError
            NestEncode _ -> notDecodeError
            WithEncode _ _ -> notDecodeError
          parseNest :: forall c cs'. CsvMap cs' c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . to <$> gParseRecord @_ @c proxy# cm (V.drop idx record)
          parseNest (CsvEncode _) _ = notDecodeError
          idx = index @_ @s fieldMapping
          selector = symbolVal @s undefined
          notDecodeError :: C.Parser f
          notDecodeError = error $ "Field " <> selector <> " is not a decode field"