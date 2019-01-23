{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Types.Codec(Codec(..), idCodec) where

import GHC.OverloadedLabels
import GHC.Records

import qualified Data.Csv as C
import Data.Profunctor
import GHC.TypeLits

-- Initially expose our field type so that it can be mapped over
-- r - Record type
-- f - field type with record
-- d - Type to decode as
-- e - type to encode as
data Codec f d e = Codec
  {  encoder :: f -> e
  , decoder :: d -> f
  }

instance Profunctor (Codec f) where
  dimap d e fm = fm
    { encoder = e . encoder fm
    , decoder = decoder fm . d
    }

idCodec :: (f~d, f~e) => Codec f d e
idCodec = Codec id id

instance (f~d, f~e) => IsLabel x (Codec f d e) where
  fromLabel = Codec id id
