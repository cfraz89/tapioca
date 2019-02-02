{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Tapioca.Internal.Types.FieldCodec(FieldCodec(..), (<:>), codec) where

import Control.Lens
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

--import qualified Data.Invertible as Inv
--import Data.Invertible.Bijection

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - type to encode and decode as
data FieldCodec (s :: Symbol) r f c = FieldCodec
  { _field :: Getter r f
  , _codec :: Iso' f c
  }

infixl 4 <:>
(<:>) :: FieldCodec s r f c -> Iso' f c' -> FieldCodec s r f c'
fc <:> c = fc { _codec = c }

codec :: FieldCodec s r f c -> Iso' f c' -> FieldCodec s r f c'
codec fc c = fc <:> c

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (FieldCodec x' r' f' c) where
  fromLabel = FieldCodec (to $ getField @x) (iso id id)
