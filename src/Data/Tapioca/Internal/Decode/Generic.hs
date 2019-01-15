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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.Tapioca.Internal.Decode.Generic
  (  GFieldIndex
  ) where

import GHC.Exts
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Data.Type.Equality
import Data.Kind

--type GenericCsvDecode r = (GSelectorList (Rep r), GParseRecord (Rep r), Generic r)

--data SelProxy t f a = SelProxy

-- class GSelectorList f where
--   gSelectorList :: [String]

-- instance GSelectorList f => GSelectorList (M1 D t f) where
--   gSelectorList = gSelectorList @f

-- instance GSelectorList f => GSelectorList (M1 C t f) where
--   gSelectorList = gSelectorList @f

-- instance Selector t => GSelectorList (M1 S t f) where
--   gSelectorList = [selName (SelProxy @t @f)]

-- instance (GSelectorList a, GSelectorList b) => GSelectorList (a :*: b) where
--   gSelectorList = gSelectorList @a <> gSelectorList @b

-- data SelectorData = forall a. Typeable a => SelectorData a

-- class GParseRecord f where
--   gParseRecord :: V.Vector SelectorData -> C.Parser (f p)

-- class GParseSelector f where
--   gParseSelector :: Int -> V.Vector SelectorData -> C.Parser (f p)

-- instance GParseRecord f => GParseRecord (M1 D t f) where
--   gParseRecord selectorMetas = M1 <$> gParseRecord selectorMetas

-- instance GParseSelector f => GParseRecord (M1 C t f) where
--   gParseRecord selectorMetas = M1 <$> gParseSelector 0 selectorMetas

-- instance Typeable a => GParseSelector (M1 S m (K1 i a)) where
--   gParseSelector i selectorMetas = M1 . K1 <$> parseSelector (selectorMetas V.! i)
--     where parseSelector (SelectorData sd) = case testEquality (typeOf sd) (typeRep @a) of
--             Just Refl -> pure sd
--             _ -> fail $ "Type mismatch. This shouldn't happen! " <> show (typeOf sd) <> " - " <> show (typeRep @a)

-- instance (GParseSelector a, GParseSelector b) => GParseSelector (a :*: b) where
--   gParseSelector i selectorMetas = do
--     a <- gParseSelector i selectorMetas
--     b <- gParseSelector (succ i) selectorMetas
--     pure $ a :*: b

type family BoolNat (a :: Bool) where
  BoolNat 'False = 'Nothing
  BoolNat 'True = 'Just 0

type family AddMaybe (n :: Maybe Nat) (i :: Nat) where
  AddMaybe 'Nothing i = 'Nothing
  AddMaybe ('Just a) i = 'Just (a + i)

-- type family Alt (x1 :: Symbol) (m1 :: Maybe a) (x2 :: Symbol) (m2 :: Maybe a) where
--   Alt x1 ('Just i1) x2 b = '(x1, ('Just i1))
--   Alt x1 'Nothing x2 i2 = '(x2, i2)
  
class Alt (m1 :: Maybe a) (m2 :: Maybe a) (mv :: Maybe a) | m1 m2 -> mv where
instance Alt ('Just i1) m2 ('Just i1)
instance forall (i2 :: Nat) (i3 :: Nat). i3 ~ (i2 + 1) => Alt 'Nothing ('Just i2) ('Just i3)

class GFieldIndex (x :: Symbol) r (i :: Maybe Nat)

instance GFieldIndex x f i => GFieldIndex x (M1 D t f) i
instance GFieldIndex x f i => GFieldIndex x (M1 C t f) i
instance n ~ BoolNat ('Just x == s) => GFieldIndex x (M1 S ('MetaSel s p1 p2 p3) f) n
instance (GFieldIndex x f1 i1, GFieldIndex x f2 i2, Alt i1 i2 i) => GFieldIndex x (f1 :*: f2) i

--class FieldIndex (x :: Symbol) r (i :: Nat) | x r -> i
--instance (GFieldIndex x (Rep r) ('Just i), KnownNat i) => FieldIndex x r i

type HasFieldIndex (x :: Symbol) r (i :: Nat) = (GFieldIndex x (Rep r) ('Just i), KnownNat i)

data Dummy = Dummy { dt :: Int, dt2 :: String } deriving Generic

varIndex :: forall s a i. (KnownNat i, GFieldIndex s (Rep Dummy) a, a ~ 'Just i) => Integer
varIndex = natVal' @i proxy#

hasIndexTest :: forall s i. HasFieldIndex s Dummy i => Integer
hasIndexTest = natVal' @i proxy#
