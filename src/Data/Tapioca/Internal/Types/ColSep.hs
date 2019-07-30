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
import Data.Type.Bool
import GHC.TypeLits

import Data.Tapioca.Internal.Types.Index
import Data.Tapioca.Internal.Types.Match
import Data.Tapioca.Internal.Types.HFoldable
import Data.Tapioca.Internal.Types.CsvMapType

-- Our joining/induction type for records
infixl 1 :|

-- | Joins together FieldMappings to create a CsvMap
data (a :| b) (t :: CsvMapType) r = a t r :| b t r

-- data Cols r xs where
--   (:$) :: Cols r '[]
--   (:|) :: x -> Cols r (x ': xs)

-- | Induction over :|
-- instance {-# Overlapping #-} (HFoldable t x, HFoldable ts x) => HFoldable (Cols r xs) x where
instance {-# Overlapping #-} (HFoldable (t mt r) x, HFoldable (ts mt r) x) => HFoldable ((t :| ts) mt r) x where
  hFoldl f b (x :| xs) = hFoldl f (hFoldl f b x) xs
  hFoldr f b (x :| xs) = hFoldr f (hFoldr f b  x) xs
  hFoldMap f (x :| xs) = hFoldMap f x <> hFoldMap f xs

type instance Match ((t1 :| t2) mt r) s = Match (t1 mt r) s || Match (t2 mt r) s

instance (Width (t1 mt r), Width (t2 mt r)) => Width ((t1 :| t2) mt r) where
  width (t1 :| t2) = width t1 + width t2

-- | Class to decide on whether to progress to next segment based on selector matching of first
class PickNext (t :: Type) (m :: Bool) where
  type Next t m :: Type
  incr :: Head t -> Int
  next :: t -> Next t m

instance PickNext ((t1 :| t2) mt r) 'True where
  type Next ((t1 :| t2) mt r) 'True = t1 mt r
  incr _ = 0
  next (t1 :| _) = t1

instance Width (t1 mt r) => PickNext ((t1 :| t2) mt r) 'False where
  type Next ((t1 :| t2) mt r) 'False = t2 mt r
  incr = width
  next (_ :| t2) = t2

type family Head t where
  Head ((t1 :| t2) mt r) = t1 mt r

instance (m ~ Match (t1 mt r) s, PickNext ((t1 :| t2) mt r) m, Index (Next ((t1 :| t2) mt r) m) s) => Index ((t1 :| t2) mt r) s where
 index t@(x :| _) = incr @((t1 :| t2) mt r) @m x + index @_ @s (next @_ @m t)

-- Return t1 if provided with true, otherwise t2
class PickMatch (t :: Type) (b1 :: Bool) (b2 :: Bool) (s :: Symbol) where
  type Picked t b1 b2 s :: Type
  picked :: t -> Picked t b1 b2 s

instance PickMatch ((t1 :| t2) mt r) 'True b2 s where
  type Picked ((t1 :| t2) mt r) 'True b2 s = t1 mt r
  picked (t1 :| _) = t1

instance PickMatch ((t1 :| t2) mt r) 'False 'True s where
  type Picked ((t1 :| t2) mt r) 'False 'True s = t2 mt r
  picked (_ :| t2) = t2

instance PickMatch t 'False 'False s where
  type Picked t 'False 'False s = TypeError ('Text "No mapping provided for field '" ':<>: 'Text s ':<>: 'Text "'!\r\n"
                                                 ':<>: 'Text "\tAll fields must exist in a mapping")
  picked _ = undefined
