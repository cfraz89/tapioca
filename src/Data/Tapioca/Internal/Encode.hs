{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tapioca.Internal.Encode
  ( toRecord
  , toNamedRecord
  , header
  ) where

import Data.Tapioca.Internal.Types.Mapping

import Control.Monad.Reader
import qualified Data.Csv as C
import qualified Data.Vector as V


-- toNamedRecord record = foldMap namedRecord (unCsvMap csvMap)
--     where namedRecord (name := fm) = HM.singleton name (C.toField $ encoder fm record)
--           namedRecord (Splice fm) = toNamedRecord $ encoder fm record
