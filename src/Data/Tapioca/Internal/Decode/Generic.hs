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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Decode.Generic
  ( GenericCsvDecode
  , GSelectorList(..)
  , GParseRecord(..)
  , GParseSelector
  , SelectorData(..)
  , HasFieldI(..)
  ) where

import GHC.Generics
import GHC.TypeLits
import GHC.Exts

import qualified Data.Csv as C
import Data.Proxy
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

data Dummy = Dummy { dt :: Int, dt2 :: String }
  deriving Generic

class HasFieldI (x :: Symbol) (i :: INat) r a | x i r -> a where
  getField :: r -> a

type family UnRep r where
  UnRep (M1 S s (K1 i a)) = a
  UnRep (M1 C t f) = UnRep f
  -- UnRep (f :*: f2) = UnRep f2 -- not strictly corect
  UnRep (M1 D t f) = UnRep f
  
class GHasField (x :: Symbol) (r :: * -> *) a | x r -> a where
  gGetField :: r p -> a

class GHasFieldI (x :: Symbol) (i :: INat) (r :: * -> *) a | x i r -> a, x r -> a where
  gGetFieldI :: r p -> a

data INat = Zero | Succ INat

instance x~s => GHasField x (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 inf a)) a where
  gGetField (M1 (K1 a)) = a

instance GHasField x f a => GHasField x (M1 C t f) a where
  gGetField (M1 f) = gGetField @x f

instance GHasFieldI x i f a => GHasFieldI x i (M1 C t f) a where
  gGetFieldI (M1 f) = gGetFieldI @x @i f

instance x ~ s => GHasFieldI x 'Zero (M1 S ('MetaSel ('Just s) p1 p2 p3) (K1 i' a)) a where
  gGetFieldI f = gGetField @x f

instance {-# OVERLAPPABLE #-} GHasFieldI x i f a => GHasFieldI x i (f :*: f2) a where
  gGetFieldI (f :*: _) = gGetFieldI @x @i @f @a f

instance {-# OVERLAPPING #-} (GHasFieldI x i f1 a1, GHasField x2 f2 a2) => GHasFieldI x2 ('Succ i) (f1 :*: f2) a2 where
  gGetFieldI (_ :*: f2) = gGetField @x2 @f2 @a2 f2

instance GHasFieldI x i f a => GHasFieldI x i (M1 D t f) a where
  gGetFieldI (M1 f) = gGetFieldI @x @i f

instance (Generic r, GHasFieldI x i (Rep r) a) => HasFieldI x i r a where
  getField record = gGetFieldI @x @i @(Rep r) @a (from record)