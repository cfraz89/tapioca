{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , GSelectorList(..)
  , GParseRecord(..)
  , GParseSelector
  , SelectorMeta(..)
  ) where

import GHC.Generics

import Data.Tapioca.Internal.ReifyRecord

import qualified Data.Csv as C
import Data.Type.Equality
import qualified Data.Vector as V
import Type.Reflection

type GenericCsvDecode r rc = (GSelectorList (Rep r), GParseRecord (Rep r) rc, Generic r)

data SelProxy t f a = SelProxy

class GSelectorList f where
  gSelectorList :: [String]

instance GSelectorList f => GSelectorList (M1 D t f) where
  gSelectorList = gSelectorList @f

instance GSelectorList f => GSelectorList (M1 C t f) where
  gSelectorList = gSelectorList @f

instance Selector t => GSelectorList (M1 S t f) where
  gSelectorList = [selName (SelProxy @t @f)]

instance (GSelectorList a, GSelectorList b) => GSelectorList (a :*: b) where
  gSelectorList = gSelectorList @a <> gSelectorList @b

-- rc is C.Record or C.NamedRecord
data SelectorMeta rc = forall f d. (C.FromField d, Typeable f) => Field (TypeRep f) (RecordIndex rc) (d -> f)
                    | forall r d. (GenericCsvDecode d rc, Typeable r) => Record (TypeRep r) (V.Vector (SelectorMeta rc)) (d -> r)

class GParseRecord f rc where
  gParseRecord :: V.Vector (SelectorMeta rc) -> rc -> C.Parser (f p)

class GParseSelector f rc where
  gParseSelector :: Int -> V.Vector (SelectorMeta rc) -> rc -> C.Parser (f p)

instance GParseRecord f rc => GParseRecord (M1 D t f) rc where
  gParseRecord selectorMetas record = M1 <$> gParseRecord selectorMetas record

instance GParseSelector f rc => GParseRecord (M1 C t f) rc where
  gParseRecord selectorMetas record = M1 <$> gParseSelector 0 selectorMetas record

instance (Typeable a, ReifyRecord rc, Show (RecordIndex rc)) => GParseSelector (M1 S m (K1 i a)) rc where
  gParseSelector i selectorMetas record = M1 . K1 <$> parseSelector (selectorMetas V.! i)
    where parseSelector (Field tr idx decodeMapper)
            | Just Refl <- testEquality tr (typeRep @a) = case lookupRecord record idx of
                Just field -> decodeMapper <$> C.parseField field
                Nothing -> fail $ "Mapping of field with index " <> show idx <> " not in record"
          parseSelector (Record dataRep metas decodeMapper)
            | Just Refl <- testEquality dataRep (typeRep @a) = decodeMapper . to <$> gParseRecord metas record
          parseSelector _ = fail "Type mismatch. This shouldn't happen!"
  
instance (GParseSelector a r, GParseSelector b r) => GParseSelector (a :*: b) r where
  gParseSelector i selectorMetas record = do
    a <- gParseSelector i selectorMetas record
    b <- gParseSelector (succ i) selectorMetas record
    pure $ a :*: b