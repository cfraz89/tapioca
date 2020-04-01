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
import GHC.Generics as G
import GHC.TypeLits

import Data.ByteString (ByteString)
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Semigroup ((<>))
import Data.String.Conv

instance (Can 'Decode cs, Reduce t ('Just s) ('Just s) f cs r, KnownSymbol s) => GParseRecord (S1 ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @('Just s) @('Just s) @f @cs @r fieldMapping of
            BicodeFM name (Field _ _codec) -> decodeWith name (_decode _codec)
            DecodeFM name (DecodeField decoder') -> decodeWith name decoder'
            Nest (Field _ _codec :: Field s f c EncodeDecode r) -> parseNest (csvMap @_ @c) (_decode _codec)
            NestDecode (DecodeField decoder' :: Field s f c Decode r) -> parseNest (csvMap @_ @c) decoder'
            With (Field _ _codec :: Field s f c EncodeDecode r) cm -> parseNest cm (_decode _codec)
            WithDecode (DecodeField decoder' :: Field s f c Decode r) cm -> parseNest cm decoder'
            EncodeFM _ _ -> notDecodeError
            NestEncode _ -> notDecodeError
            WithEncode _ _ -> notDecodeError
          decodeWith :: C.FromField c => ByteString -> (c -> f) -> C.Parser f
          decodeWith name decoder' = maybe (fail errMsg) decode val
              where errMsg = "No column " <> toS name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup (toS name) namedRecord
                    decode :: C.Field -> C.Parser f
                    decode f = decoder' <$> C.parseField f
          parseNest :: forall c cs'. Can 'Decode cs => CsvMap cs' c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . G.to <$> gParseRecord @_ @c proxy# cm namedRecord
          parseNest (CsvDecode cm) dec = dec . G.to <$> gParseRecord @_ @c proxy# cm namedRecord
          parseNest (CsvEncode _) _ = notDecodeError
          selector = symbolVal @s undefined
          notDecodeError :: C.Parser f
          notDecodeError = error $ "Field " <> selector <> " is not a decode field"

instance (Can 'Decode cs, Reduce t ('Just s) ('Just s) f cs r, Index t s, KnownSymbol s) => GParseRecord (S1 ('MetaSel ('Just s) p1 p2 p3) (Rec0 f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @('Just s) @('Just s) @f @cs @r fieldMapping of
            BicodeFM _ (Field _ _codec) -> decodeWith (_decode _codec)
            DecodeFM _ (DecodeField decoder') -> decodeWith decoder'
            Nest (Field _ _codec :: Field s f c EncodeDecode r) -> parseNest (csvMap @_ @c) (_decode _codec)
            NestDecode (DecodeField decoder' :: Field s f c Decode r) -> parseNest (csvMap @_ @c) decoder'
            With (Field _ _codec :: Field s f c EncodeDecode r) cm -> parseNest cm (_decode _codec)
            WithDecode (DecodeField decoder' :: Field s f c Decode r) cm -> parseNest cm decoder'
            EncodeFM _ _ -> notDecodeError
            NestEncode _ -> notDecodeError
            WithEncode _ _ -> notDecodeError
          decodeWith :: C.FromField c => (c -> f) -> C.Parser f
          decodeWith decoder' = maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    val = record V.!? idx
                    decode :: C.Field -> C.Parser f
                    decode f = decoder' <$> C.parseField f
          parseNest :: forall c cs'. CsvMap cs' c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . G.to <$> gParseRecord @_ @c proxy# cm (V.drop idx record)
          parseNest (CsvDecode cm) dec = dec . G.to <$> gParseRecord @_ @c proxy# cm (V.drop idx record)
          parseNest (CsvEncode _) _ = notDecodeError
          idx = index @_ @s fieldMapping
          selector = symbolVal @s undefined
          notDecodeError :: C.Parser f
          notDecodeError = error $ "Field " <> selector <> " is not a decode field"

instance (Can 'Decode cs, Reduce t 'Nothing 'Nothing f cs r)  => GParseRecord (C1 x (S1 ('MetaSel ms p1 p2 p3) (Rec0 f))) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @'Nothing @'Nothing @f @cs @r fieldMapping of
            Coerced cm -> parseCoerced cm
            CoercedDecode cm -> parseCoerced cm
            CoercedEncode _ -> error "Not a decode coercion"
          parseCoerced :: forall cs'. Can 'Decode cs => CsvMap cs' f -> C.Parser f
          parseCoerced (CsvMap cm) = G.to <$> gParseRecord @_ @f proxy# cm namedRecord
          parseCoerced (CsvDecode cm) = G.to <$> gParseRecord @_ @f proxy# cm namedRecord
          parseCoerced (CsvEncode _) = error "Cannot coerce an encode map for decoding"

instance (Can 'Decode cs, Reduce t 'Nothing 'Nothing f cs r)  => GParseRecord (C1 x (S1 ('MetaSel ms p1 p2 p3) (Rec0 f))) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @'Nothing @'Nothing @f @cs @r fieldMapping of
            Coerced cm -> parseCoerced cm
            CoercedDecode cm -> parseCoerced cm
            CoercedEncode _ -> error "Not a decode coercion"
          parseCoerced :: forall cs'. Can 'Decode cs => CsvMap cs' f -> C.Parser f
          parseCoerced (CsvMap cm) = G.to <$> gParseRecord @_ @f proxy# cm record
          parseCoerced (CsvDecode cm) = G.to <$> gParseRecord @_ @f proxy# cm record
          parseCoerced (CsvEncode _) = error "Cannot coerce an encode map for decoding"

-- instance GParseRecord f r t i => GParseRecord (C1 x f) r t i where