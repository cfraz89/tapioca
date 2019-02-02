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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Tapioca.Internal.Types.ParseRecord where

import Data.Tapioca.Internal.Types.Codec
import Data.Tapioca.Internal.Types.CsvMap
import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.GParseRecord
import Data.Tapioca.Internal.Common (bsVectorString)

import GHC.Exts
import GHC.Generics

import Control.Invertible.Monoidal
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

instance Reduce t s r f => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.NamedRecord where
  gParseRecord _ fieldMapping namedRecord = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            Field name cdc -> maybe (fail errMsg) decode val
              where errMsg = "No column " <> BC.unpack name <> " in columns: " <> bsVectorString (HM.keys namedRecord)
                    val = HM.lookup name namedRecord
                    decode = (biFrom (_codec cdc) <$>) . C.parseField
            Nest (cdc :: Codec s r _ c) -> parseNest (csvMap @c)
              where parseNest (CsvMap cm) = biFrom (_codec cdc) . to <$> gParseRecord @_ @c proxy# cm namedRecord

instance (Reduce t s r f, Index t s) => GParseRecord (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i f)) r t C.Record where
  gParseRecord _ fieldMapping record = M1 . K1 <$> parseByType
    where parseByType = case selectorMapping @_ @s @r @f fieldMapping of
            Field _ cdc -> maybe (fail errMsg) decode val
              where errMsg = "Can't parse item at index " <> show (index @_ @s fieldMapping) <> " in row: " <> bsVectorString (V.toList record)
                    decode = (biFrom (_codec cdc) <$>) . C.parseField
                    --idx = index
                    val = record V.!? index @_ @s fieldMapping
            Nest (cdc :: Codec s r _ c) -> parseNest (csvMap @c)
              where parseNest (CsvMap cm) = biFrom (_codec cdc) . to <$> gParseRecord @_ @c proxy# cm record
