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
  (
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



