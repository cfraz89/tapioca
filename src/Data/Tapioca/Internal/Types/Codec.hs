{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Types.Codec(Codec(..)) where

import GHC.OverloadedLabels
import GHC.Records

import Data.Profunctor
import GHC.TypeLits

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- d - Type to decode as
-- e - type to encode as
data Codec (s :: Symbol) r f d e = Codec
  { getF :: r -> f
  , encoder :: f -> e
  , decoder :: d -> f
  }

instance Profunctor (Codec s r f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }

instance (f~d, f~e, HasField x r f, x~x', r~r', f~f') => IsLabel x (Codec x' r' f' d e) where
  fromLabel = Codec (getField @x) id id
