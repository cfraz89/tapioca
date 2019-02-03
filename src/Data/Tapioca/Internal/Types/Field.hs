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

module Data.Tapioca.Internal.Types.Field (Field(..), (<:>), codec) where

import Control.Lens
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - type to encode and decode as
data Field (s :: Symbol) r f c = Field
  { _field :: Getter r f
  , _codec :: Iso' f c
  }

infixl 4 <:>
(<:>) :: Field s r f c -> Iso' f c' -> Field s r f c'
fc <:> c = fc { _codec = c }

codec :: Field s r f c -> Iso' f c' -> Field s r f c'
codec fc c = fc <:> c

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Field x' r' f' c) where
  fromLabel = Field (to $ getField @x) (iso id id)
