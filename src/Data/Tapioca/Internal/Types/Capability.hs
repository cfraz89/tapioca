{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tapioca.Internal.Types.Capability 
  ( Capability(..)
  , Only
  , Encode
  , Decode
  , EncodeDecode
  , Can
  ) where

import Data.Type.Bool
import Data.Type.Equality

data Capability = Encode | Decode

type Only (a :: k) = a ': '[]

type Encode = Only 'Encode
type Decode = Only 'Decode
type EncodeDecode = ['Encode, 'Decode]

type family Can' c cs where
  Can' c '[] = 'False
  Can' c (c' ': cs') = c == c' || Can' c cs'

class Can c cs
instance Can' c cs ~ 'True => Can c cs