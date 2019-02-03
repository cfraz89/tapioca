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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Tapioca.Internal.Types.ParseRecord where

import Data.Tapioca.Internal.Types.Field
import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.GParseRecord
import Data.Tapioca.Internal.Common (bsVectorString)

import GHC.Exts
import GHC.Generics

import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Semigroup ((<>))

instance Reduce t s r f => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            MapField name Field{..} -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode = (L.view (L.from _codec) <$>) . C.parseField
            Nest (Field{..} :: Field s r _ c) -> parseNest (csvMap @c)
              where parseNest (CsvMap cm) = L.view (L.from _codec) . to <$> gParseRecord @_ @c proxy# cm namedRecord

instance (Reduce t s r f, Index t s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            MapField _ Field{..} -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show idx <> " in row: " <> bsVectorString (V.toList record)
                    decode = (L.view (L.from _codec) <$>) . C.parseField
                    val = record V.!? idx
            Nest (Field{..} :: Field s r _ c) -> parseNest (csvMap @c)
              where parseNest (CsvMap cm) = L.view (L.from _codec) . to <$> gParseRecord @_ @c proxy# cm (V.drop idx record)
          idx = index @_ @s fieldMapping
