{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Tapioca.Internal.Decode.Generic
( GenericCsvDecode
, GSelectorList(..)
, GParseRecord(..)
, GParseSelector
, SelectorMeta(..)
  ) where 

import GHC.Generics

import qualified Data.Csv as C
import qualified Data.Vector as V
import Data.Type.Equality
import Type.Reflection

type GenericCsvDecode r = (GSelectorList (Rep r), GParseRecord (Rep r), Generic r)

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

data SelectorMeta = forall f d. (C.FromField d, Typeable f) => Field (TypeRep f) Int (d -> f)
                 | forall r d. (GenericCsvDecode d, Typeable r) => Record (TypeRep r) (V.Vector SelectorMeta) (d -> r)

instance Show SelectorMeta where
  show (Field _ i _) = "Field " <> show i
  show (Record _ sms _) = "Record " <> show sms

class GParseRecord f where
  gParseRecord :: V.Vector SelectorMeta -> C.Record -> C.Parser (f p)

class GParseSelector f where
  gParseSelector :: Int -> V.Vector SelectorMeta -> C.Record -> C.Parser (Int, f p)

instance GParseRecord f => GParseRecord (M1 D t f) where
  gParseRecord selectorMetas record = M1 <$> gParseRecord selectorMetas record

instance GParseSelector f => GParseRecord (M1 C t f) where
  gParseRecord selectorMetas record = M1 . snd <$> gParseSelector 0 selectorMetas record

instance Typeable a => GParseSelector (M1 S m (K1 i a)) where
  gParseSelector i selectorMetas record = fmap (M1 . K1) . (succ i,) <$> parseSelector (selectorMetas V.! i) record

instance (GParseSelector a, GParseSelector b) => GParseSelector (a :*: b) where
  gParseSelector i selectorMetas record = do
    (ia, a) <- gParseSelector i selectorMetas record
    (ib, b) <- gParseSelector ia selectorMetas record
    pure (ib, a :*: b)

parseSelector :: forall a. Typeable a => SelectorMeta -> C.Record -> C.Parser a
parseSelector (Field tr pos decodeMapper) record
  | Just Refl <- testEquality tr (typeRep @a) = decodeMapper <$> C.parseField (record V.! pos)
parseSelector (Record tr metas decodeMapper) record
  | Just Refl <- testEquality tr (typeRep @a) = decodeMapper . to <$> gParseRecord metas record
parseSelector _ _ = fail "Type mismatch. This shouldn't happen!"
