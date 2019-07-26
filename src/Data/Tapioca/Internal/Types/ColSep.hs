{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
data a :| b = a :| b

-- | Induction over :|
instance {-# Overlapping #-} (HFoldable t x, HFoldable ts x) => HFoldable (t :| ts) x where
  hFoldl f b (x :| xs) = hFoldl f (hFoldl f b x) xs
  hFoldr f b (x :| xs) = hFoldr f (hFoldr f b  x) xs
  hFoldMap f (x :| xs) = hFoldMap f x <> hFoldMap f xs

type instance Match (t1 :| t2) s = Match t1 s || Match t2 s

instance (Width t1, Width t2) => Width (t1 :| t2) where
  width (t1 :| t2) = width t1 + width t2

instance (m ~ Match t1 s, PickNext t1 t2 m, Index (Next t1 t2 m) s) => Index (t1 :| t2) s where
  index (t1 :| t2) = incr @_ @t2 @m t1 + index @_ @s (next @_ @_ @m t1 t2)

-- Return t1 if provided with true, otherwise t2
class PickMatch (t1 :: Type) (t2 :: Type) (b1 :: Bool) (b2 :: Bool) (s :: Symbol) where
  type Picked t1 t2 b1 b2 s :: Type
  picked :: t1 :| t2 -> Picked t1 t2 b1 b2 s

instance PickMatch t1 t2 'True b2 s where
  type Picked t1 t2 'True b2 s = t1
  picked (t1 :| _) = t1

instance PickMatch t1 t2 'False 'True s where
  type Picked t1 t2 'False 'True s = t2
  picked (_ :| t2) = t2

instance PickMatch t1 t2 'False 'False s where
  type Picked t1 t2 'False 'False s = TypeError ('Text "No mapping provided for field '" ':<>: 'Text s ':<>: 'Text "'!\r\n"
                                                 ':<>: 'Text "\tAll fields must exist in a mapping")
  picked _ = undefined
