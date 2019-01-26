{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Data.Tapioca.Internal.Types.Separator
  ((:|)(..),
   HFoldable(..)
  ) where

import Data.Kind

-- | Our joining type for csv Maps
infixl 1 :|
data a :| b = a :| b


class HFoldable (t :: Type) x where
  foldVal :: t -> x
  hFoldr :: (forall t'. HFoldable t' x => x -> b -> b) -> b -> t -> b
  hFoldMap :: Monoid m => (forall t'. HFoldable t' x => x -> m) -> t -> m
  hFoldl :: (forall t'. HFoldable t' x => b -> x -> b) -> b -> t -> b

instance (HFoldable t x, HFoldable ts x) => HFoldable (t :| ts) x where
  foldVal (x :| _) = foldVal x
  hFoldl f b (x :| xs) = hFoldl (f @ts) (f @t b (foldVal x)) xs
  hFoldr f b (x :| xs) = hFoldr (f @ts) (f @t (foldVal x) b) xs
  hFoldMap f (x :| xs) = f @t (foldVal x) <> hFoldMap (f @ts) xs
