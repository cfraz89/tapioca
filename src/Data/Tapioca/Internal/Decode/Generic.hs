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

module Data.Tapioca.Internal.Decode.Generic
  ( INat(..)
  , FieldIndex
  ) where

import GHC.Exts
import GHC.Generics
import GHC.TypeLits

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

-- Using peanos gets around the duplicate instance definition against the :*: instances
data INat = Z | S INat

class INatVal (n :: INat) where
  iNatVal :: Proxy# n -> Int

instance INatVal 'Z where
  iNatVal _ = 0

instance INatVal n => INatVal ('S n) where
  iNatVal _ = 1 + iNatVal @n proxy#

type family ToNat (a :: INat) where
  ToNat 'Z = 0
  --ToNat ('S n) = 1 + (ToNat n)
  ToNat ('S 'Z) = 1
  ToNat ('S ('S 'Z)) = 2


class GFieldIndex (x :: Symbol) r (i :: INat) | x r -> i

instance GFieldIndex x f i => GFieldIndex x (M1 D t f) i
instance GFieldIndex x f i => GFieldIndex x (M1 C t f) i
instance GFieldIndex x (M1 S ('MetaSel ('Just x) p1 p2 p3) f) 'Z
instance {-# OVERLAPPABLE #-} GFieldIndex x1 f1 i1 => GFieldIndex x1 (f1 :*: f2) i1
instance {-# OVERLAPPING #-} GFieldIndex x2 f2 i2 => GFieldIndex x2 (f1 :*: f2) ('S i2)

class FieldIndex (x :: Symbol) r (i :: Nat) | x r -> i

--instance (GFieldIndex x (Rep r) n, n' ~ ToNat n, KnownNat n') => FieldIndex x r n'

-- >>> :set -XKindSignatures
-- >>> :set -XDeriveGeneric
-- >>> :set -XRankNTypes
-- >>> :set -XDeriveGeneric
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XAllowAmbiguousTypes
-- >>> :set -XMagicHash
-- >>> :set -XTypeApplications
-- >>>
-- >>> import GHC.Exts
data Dummy = Dummy { dt :: Int, dt2 :: String } deriving Generic

test :: forall a. (GFieldIndex "dt" (Rep Dummy) a) => Int
-- test = iNatVal @'Z proxy#
test = 0
-- >>>
-- >>> print test
