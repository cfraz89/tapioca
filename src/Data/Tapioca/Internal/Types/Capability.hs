{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Tapioca.Internal.Types.Capability 
  ( Capability(..)
  , Only
  , Encode
  , Decode
  , EncodeDecode
  , Can
  ) where

import Data.Constraint
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

data Capability = Encode | Decode

type Only (a :: k) = a ': '[]

type Encode = Only 'Encode
type Decode = Only 'Decode
type EncodeDecode = ['Encode, 'Decode]

class Can c cs

type family Has c cs where
  Has c '[] = 'False
  Has c (c' ': cs') = c == c' || Has c cs'

type family EnsureCan c cs t :: Constraint where
  EnsureCan c cs 'True = ()
  EnsureCan c cs 'False = TypeError ('Text "No capability " ':<>: 'ShowType c 
                                    ':<>: 'Text " in type's capabilities" ':<>: 'ShowType cs)

instance EnsureCan c cs (Has c cs) => Can c cs
