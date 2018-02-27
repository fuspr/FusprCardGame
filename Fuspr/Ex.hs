{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module Fuspr.Ex
  ( Some(..)
  ) where

import Data.Type.Equality

data Some f where
  Some :: f a -> Some f

instance TestEquality f => Eq (Some f) where
  Some x == Some y =
    case testEquality x y of
      Just Refl -> True
      Nothing   -> False
