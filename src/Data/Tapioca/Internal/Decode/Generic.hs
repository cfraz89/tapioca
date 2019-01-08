{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , GSelectorList(..)
  , GParseRecord(..)
  , GParseNamedRecord(..)
  , GParseSelector
  , SelectorMeta(..)
  ) where

import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import Data.Type.Equality
import qualified Data.Vector as V
import Type.Reflection

type GenericCsvDecode r = (GSelectorList (Rep r), GParseRecord (Rep r), GParseNamedRecord (Rep r), Generic r)

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

-- a is container type used in sub records
data SelectorMeta = forall f d. (C.FromField d, Typeable f) => Field (TypeRep f) B.ByteString Int (d -> f)
                  | forall r d. (GenericCsvDecode d, Typeable r) => Record (TypeRep r) (V.Vector SelectorMeta) (d -> r)

class GParseRecord f where
  gParseRecord :: V.Vector SelectorMeta -> C.Record -> C.Parser (f p)

class GParseSelector f where
  gParseSelector :: Int -> V.Vector SelectorMeta -> C.Record -> C.Parser (f p)

class GParseNamedRecord f where
  gParseNamedRecord :: V.Vector SelectorMeta -> C.NamedRecord -> C.Parser (f p)

class GParseNamedSelector f where
  gParseNamedSelector :: Int -> V.Vector SelectorMeta -> C.NamedRecord -> C.Parser (f p)

-- For FromRecord
instance GParseRecord f => GParseRecord (M1 D t f) where
  gParseRecord selectorMetas record = M1 <$> gParseRecord selectorMetas record

instance GParseSelector f => GParseRecord (M1 C t f) where
  gParseRecord selectorMetas record = M1 <$> gParseSelector 0 selectorMetas record

instance Typeable a => GParseSelector (M1 S m (K1 i a)) where
  gParseSelector i selectorMetas record = M1 . K1 <$> parseSelector (selectorMetas V.! i)
    where parseSelector (Field tr _ pos decodeMapper)
            | Just Refl <- testEquality tr (typeRep @a) = decodeMapper <$> C.parseField (record V.! pos)
          parseSelector (Record tr metas decodeMapper)
            | Just Refl <- testEquality tr (typeRep @a) = decodeMapper . to <$> gParseRecord metas record
          parseSelector _ = fail "Type mismatch. This shouldn't happen!"

instance (GParseSelector a, GParseSelector b) => GParseSelector (a :*: b) where
  gParseSelector i selectorMetas record = do
    a <- gParseSelector i selectorMetas record
    b <- gParseSelector (succ i) selectorMetas record
    pure $ a :*: b

-- For FromNamedRecord
instance GParseNamedRecord f => GParseNamedRecord (M1 D t f) where
  gParseNamedRecord selectorMetas namedRecord = M1 <$> gParseNamedRecord selectorMetas namedRecord

instance GParseNamedSelector f => GParseNamedRecord (M1 C t f) where
  gParseNamedRecord selectorMetas namedRecord = M1 <$> gParseNamedSelector 0 selectorMetas namedRecord

instance Typeable a => GParseNamedSelector (M1 S m (K1 i a)) where
  gParseNamedSelector i selectorMetas namedRecord = M1 . K1 <$> parseSelector (selectorMetas V.! i)
    where parseSelector (Field tr fieldHeader _ decodeMapper)
            | Just Refl <- testEquality tr (typeRep @a) = case HM.lookup fieldHeader namedRecord of
              Just field -> decodeMapper <$> C.parseField field
              Nothing -> fail $ "Mapping header " <> show fieldHeader <> " not in record"
          parseSelector (Record tr metas decodeMapper)
            | Just Refl <- testEquality tr (typeRep @a) = decodeMapper . to <$> gParseNamedRecord metas namedRecord
          parseSelector _ = fail "Type mismatch. This shouldn't happen!"