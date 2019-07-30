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

import GHC.Exts
import GHC.Generics
import GHC.TypeLits

import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Semigroup ((<>))

instance (Reduce t s r f, KnownSymbol s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            BicodeFM name Field{..} -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode :: C.Field -> C.Parser f
                    decode = (_decode _codec <$>) . C.parseField
            Nest (Field{..} :: Field s f c r) -> parseNest (csvMap @_ @c) (_decode _codec)
            NestEncode _ -> fail $ "Cannot nest an encode field while decoding field: " ++ symbolVal' @s proxy#
            With (Field{..} :: Field s f c r) cm -> parseNest cm (_decode _codec)
            WithEncode _ _ -> fail $ "Cannot use an encode mapping while decoding: " ++ symbolVal' @s proxy#
            EncodeFM name _ -> fail $ "Cannot decode to a field that has been defined encode only: " ++ BC.unpack name
          parseNest :: forall c. CsvMap 'Both c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . to <$> gParseRecord @_ @c proxy# cm namedRecord

instance (Reduce t s r f, Index t s, KnownSymbol s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            BicodeFM _ Field{..} -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    decode = (_decode _codec <$>) . C.parseField
                    val = record V.!? idx
            Nest (Field{..} :: Field s f c r) -> parseNest (csvMap @_ @c) (_decode _codec)
            NestEncode _ -> fail $ "Cannot nest an encode field while decoding field:" <> symbolVal' @s proxy#
            With(Field{..} :: Field s f c r) cm -> parseNest cm (_decode _codec)
            WithEncode _ _ -> fail $ "Cannot use an encode mapping while decoding: " ++ symbolVal' @s proxy#
            EncodeFM name _ -> fail $ "Cannot decode to a field that has been defined encode only: " ++ BC.unpack name
          parseNest :: forall c. CsvMap 'Both c -> (c -> f) -> C.Parser f
          parseNest (CsvMap cm) dec = dec . to <$> gParseRecord @_ @c proxy# cm (V.drop idx record)
          idx = index @_ @s fieldMapping
