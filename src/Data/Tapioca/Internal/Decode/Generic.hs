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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

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

--class HasFieldI (x :: Symbol) (i :: INat) r a | x r -> a, i r -> a, x r -> i, i r -> x where
class HasFieldI (x :: Symbol) (i :: INat) r a where
  getField :: r -> a

type family UnRep r where
  UnRep (M1 S s (K1 i a)) = a
  UnRep (M1 C t f) = UnRep f
  UnRep (f :*: f2) = UnRep f2 -- not strictly corect
  UnRep (M1 D t f) = UnRep f
  
class GHasField (x :: Symbol) (r :: * -> *) p where
  gGetField :: Proxy x -> r p -> UnRep r

instance GHasField x (M1 S ('MetaSel ('Just x) p1 p2 p3) (K1 inf a)) p where
  gGetField _ (M1 (K1 a)) = a

class GHasFieldI (x :: Symbol) (i :: INat) (r :: * -> *) p where
  gGetFieldI :: r p -> UnRep r

data INat = Zero | Succ INat

instance (GHasField x f p) => GHasFieldI x 'Zero (M1 C t f) p where
  gGetFieldI (M1 f) = gGetField (Proxy @x) f

instance (GHasFieldI x i (M1 C t f) p, GHasField x2 f2 p) => GHasFieldI x2 ('Succ i) (M1 C t (f :*: f2)) p where
  gGetFieldI (M1 (_ :*: f)) = gGetField (Proxy @x2) f

instance (GHasFieldI x i f p) => GHasFieldI x i (M1 D t f) p where
  gGetFieldI (M1 f) = gGetFieldI @x @i f

instance (Generic r, GHasFieldI x i (Rep r) p, a ~ UnRep (Rep r)) => HasFieldI x i r a where
  getField record = gGetFieldI @x @i @(Rep r) @p (from record)