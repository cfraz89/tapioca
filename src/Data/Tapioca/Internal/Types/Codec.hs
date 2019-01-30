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

module Data.Tapioca.Internal.Types.Codec(Codec(..), codec) where

import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import qualified Data.Invertible as Inv
import Data.Invertible.Bijection

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- c - mapping to apply
data Codec (s :: Symbol) r f c = Codec
  { _getCodecField :: r -> f
  , _codec :: f <-> c
  }

codec :: (f <-> c') -> Codec s r f c -> Codec s r f c'
codec newCoder (Codec gcf _) = Codec gcf newCoder 

instance (c~f, HasField x r f, x~x', r~r', f~f') => IsLabel x (Codec x' r' f' c) where
  fromLabel = Codec (getField @x) Inv.id
