{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tapioca.Internal.Types.Index where

import Data.Kind
import GHC.TypeLits

-- | Determine how many columns a mapping consumes
-- Splices may take > 1
class Width t where
  width :: t -> Int


-- | Class for looking up position of selector in our type
-- Takes into consideration splices inserted before position
class Index t (s :: Symbol) where
  index :: t -> Int
 
-- | Class to decide on wether to progress to next segment based on selector matching of first
class PickNext (t1 :: Type) (t2 :: Type) (m :: Bool) where
  type Next t1 t2 m :: Type
  incr :: t1 -> Int
  next :: t1 -> t2 -> Next t1 t2 m

instance PickNext t1 t2 'True where
  type Next t1 t2 'True = t1
  incr _ = 0
  next t1 _ = t1 

instance Width t1 => PickNext t1 t2 'False where
  type Next t1 t2 'False = t2
  incr = width
  next _ t2 = t2
