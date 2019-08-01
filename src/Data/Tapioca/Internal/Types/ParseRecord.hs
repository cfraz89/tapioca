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
import Data.Tapioca.Internal.Types.CsvMapType

import GHC.Exts
import GHC.Generics
import GHC.TypeLits

import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Semigroup ((<>))

--Also should be Decode, as well as Both, once it exists
instance (Reduce t s f 'Both r, KnownSymbol s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @f @'Both @r fieldMapping of
            BicodeFM name (Field _ _codec) -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode :: C.Field -> C.Parser f
                    decode = (_decode _codec <$>) . C.parseField
            Nest (Field _ _codec :: Field s f c 'Both r) -> parseNest (csvMap @_ @c) (_decode _codec)
            With (Field _ _codec :: Field s f c 'Both r) cm -> parseNest cm (_decode _codec)
          parseNest :: forall c. CsvMap 'Both c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . to <$> gParseRecord @_ @c proxy# cm namedRecord

instance (Reduce t s f 'Both r, Index t s, KnownSymbol s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @f @'Both @r fieldMapping of
            BicodeFM _ (Field _ _codec) -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    decode = (_decode _codec <$>) . C.parseField
                    val = record V.!? idx
            Nest (Field _ _codec :: Field s f c 'Both r) -> parseNest (csvMap @_ @c) (_decode _codec)
            With (Field _ _codec :: Field s f c 'Both r) cm -> parseNest cm (_decode _codec)
          parseNest :: forall c. CsvMap 'Both c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . to <$> gParseRecord @_ @c proxy# cm (V.drop idx record)
          idx = index @_ @s fieldMapping
