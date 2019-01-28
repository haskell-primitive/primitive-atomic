{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Primitive.Class.Atomic
  ( PrimMach(..)
  ) where

import Data.Primitive (Prim)
import GHC.Exts

-- | Class of types supporting primitive operations that are isomorphic to
-- a machine integer. Such types support compare-and-swap and other atomic
-- operations.
class Prim a => PrimMach a where
  primMachToInt# :: a -> Int#
  primMachFromInt# :: Int# -> a

instance PrimMach Int where
  primMachToInt# (I# i) = i
  primMachFromInt# = I#

instance PrimMach Word where
  primMachToInt# (W# i) = word2Int# i
  primMachFromInt# i = W# (int2Word# i)
