{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tapioca.Internal.Types.HFoldable where

import Data.Kind
import Data.Semigroup (Semigroup)

-- | Heterogeneous folding required for encoding
class HFoldVal (t :: Type) x where
  hFoldVal :: t -> x

class HFoldable (t :: Type) x where
  hFoldr :: (x -> b -> b) -> b -> t -> b
  hFoldMap :: Semigroup m => (x -> m) -> t -> m
  hFoldl :: (b -> x -> b) -> b -> t -> b

-- | Basic fold instance
instance {-# Overlappable #-} HFoldVal t x => HFoldable t x where
  hFoldl f b x = f b (hFoldVal x)
  hFoldr f b x = f (hFoldVal x) b
  hFoldMap f x = f (hFoldVal x)
