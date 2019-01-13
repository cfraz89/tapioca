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
  , SelectorData(..)
  ) where

import GHC.Generics

import qualified Data.Csv as C
import Data.Type.Equality
import qualified Data.Vector as V
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

data SelectorData = forall a. Typeable a => SelectorData a

class GParseRecord f where
  gParseRecord :: V.Vector SelectorData -> C.Parser (f p)

class GParseSelector f where
  gParseSelector :: Int -> V.Vector SelectorData -> C.Parser (f p)

instance GParseRecord f => GParseRecord (M1 D t f) where
  gParseRecord selectorMetas = M1 <$> gParseRecord selectorMetas

instance GParseSelector f => GParseRecord (M1 C t f) where
  gParseRecord selectorMetas = M1 <$> gParseSelector 0 selectorMetas

instance Typeable a => GParseSelector (M1 S m (K1 i a)) where
  gParseSelector i selectorMetas = M1 . K1 <$> parseSelector (selectorMetas V.! i)
    where parseSelector (SelectorData sd) = case testEquality (typeOf sd) (typeRep @a) of
            Just Refl -> pure sd
            _ -> fail $ "Type mismatch. This shouldn't happen! " <> show (typeOf sd) <> " - " <> show (typeRep @a)
  
instance (GParseSelector a, GParseSelector b) => GParseSelector (a :*: b) where
  gParseSelector i selectorMetas = do
    a <- gParseSelector i selectorMetas
    b <- gParseSelector (succ i) selectorMetas
    pure $ a :*: b