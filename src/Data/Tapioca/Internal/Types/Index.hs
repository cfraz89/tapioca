{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tapioca.Internal.Types.Index where

import Data.Kind
import GHC.TypeLits

-- | Determine how many columns a mapping consumes
-- Nests may take > 1
class Width (t :: Type -> Type) r where
  width :: t r -> Int


-- | Class for looking up position of selector in our type
-- Takes into consideration splices inserted before position
class Index t (s :: Symbol) where
  index :: t -> Int

-- | Class to decide on wether to progress to next segment based on selector matching of first
class PickNext (t1 :: Type -> Type) (t2 :: Type -> Type) (r :: Type) (m :: Bool) where
  type Next t1 t2 r m :: Type
  incr :: t1 r -> Int
  next :: t1 r -> t2 r -> Next t1 t2 r m

instance PickNext t1 t2 r 'True where
  type Next t1 t2 r 'True = t1 r
  incr _ = 0
  next t1 _ = t1

instance Width t1 r => PickNext t1 t2 r 'False where
  type Next t1 t2 r 'False = t2 r
  incr = width
  next _ t2 = t2
