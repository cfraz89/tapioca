{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Data.Tapioca.Internal.Types.ColSep where

import Data.Kind
import Data.Semigroup
import Data.Type.Bool
import GHC.TypeLits

import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.HFoldable

-- Our joining/induction type for records
infixl 1 :|

-- | Joins together FieldMappings to create a CsvMap
data (a :| b) r = a r :| b r

-- data Cols r xs where
--   (:$) :: Cols r '[]
--   (:|) :: x -> Cols r (x ': xs)

-- | Induction over :|
-- instance {-# Overlapping #-} (HFoldable t x, HFoldable ts x) => HFoldable (Cols r xs) x where
instance {-# Overlapping #-} (HFoldable (t r) x, HFoldable (ts r) x) => HFoldable ((t :| ts) r) x where
  hFoldl f b (x :| xs) = hFoldl f (hFoldl f b x) xs
  hFoldr f b (x :| xs) = hFoldr f (hFoldr f b  x) xs
  hFoldMap f (x :| xs) = hFoldMap f x <> hFoldMap f xs

type instance Match (t1 :| t2) s = Match t1 s || Match t2 s

instance (Width t1 r, Width t2 r) => Width (t1 :| t2) r where
  width (t1 :| t2) = width t1 + width t2

instance (m ~ Match t1 s, PickNext t1 t2 r m, Index (Next t1 t2 r m) s) => Index ((t1 :| t2) r) s where
  index (t1 :| t2) = incr @_ @t2 @r @m t1 + index @_ @s (next @_ @_ @r @m t1 t2)

-- Return t1 if provided with true, otherwise t2
class PickMatch (t1 :: Type -> Type) (t2 :: Type -> Type) (b1 :: Bool) (b2 :: Bool) (s :: Symbol) r where
  type Picked t1 t2 b1 b2 s r :: Type
  picked :: (t1 :| t2) r -> Picked t1 t2 b1 b2 s r

instance PickMatch t1 t2 'True b2 s r where
  type Picked t1 t2 'True b2 s r = t1 r
  picked (t1 :| _) = t1

instance PickMatch t1 t2 'False 'True s r where
  type Picked t1 t2 'False 'True s r = t2 r
  picked (_ :| t2) = t2

instance PickMatch t1 t2 'False 'False s r where
  type Picked t1 t2 'False 'False s r = TypeError ('Text "No mapping provided for field '" ':<>: 'Text s ':<>: 'Text "'!\r\n"
                                                 ':<>: 'Text "\tAll fields must exist in a mapping")
  picked _ = undefined
