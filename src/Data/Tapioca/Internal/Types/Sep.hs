{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Types.Sep
  ((:|)(..)
  , EqSymbol
  , PickMatch(..)
  --, HFoldable(..)
  ) where

import GHC.TypeLits

import Data.Kind

--class HFoldable (t :: Type) where

  -- hFoldr :: (forall x. x -> b -> b) -> b -> t -> b
  -- hFoldMap :: Monoid m => (forall x. x -> m) -> t -> m

  -- hFoldl :: (b -> FieldMapping t s r f d e -> b) -> b -> (FieldMapping t s r f d e :| xs) -> b
  -- hFoldl f b (x :| xs) = hFoldl f (f b x) xs
  -- hFoldr f b (x :| xs) = hFoldr f (f x b) xs
  -- hFoldMap f (x :| xs) = f x <> hFoldMap f xs

type family OrdBool (o :: Ordering) :: Bool where
  OrdBool 'LT = 'False
  OrdBool 'EQ = 'True
  OrdBool 'GT = 'False

type EqSymbol s s' = OrdBool (CmpSymbol s s')

-- Our joining type for csv Maps
infixl 1 :|
data a :| b = a :| b

class PickMatch (t1 :: Type) (t2 :: Type) (b :: Bool) where
  type Picked t1 t2 b
  picked :: t1 :| t2 -> Picked t1 t2 b

instance PickMatch t1 t2 'True where
  type Picked t1 t2 'True = t1
  picked (t1 :| _) = t1

instance PickMatch t1 t2 'False where
  type Picked t1 t2 'False = t2
  picked (_ :| t2) = t2


