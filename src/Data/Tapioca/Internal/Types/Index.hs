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
class Width (t :: Type) where
  width :: t -> Int

-- | Class for looking up position of selector in our type
-- Takes into consideration splices inserted before position
class Index (t :: Type) (s :: Symbol) where
  index :: t -> Int
