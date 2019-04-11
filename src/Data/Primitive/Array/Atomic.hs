{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Primitive.Array.Atomic
  ( casArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState,primitive)
import Data.Primitive (MutableArray(..))
import GHC.Exts (Int(I#),casArray#,isTrue#,(==#))

-- | Given an array, an offset in Int units, the expected old value,
-- and the new value, perform an atomic compare and swap i.e. write
-- the new value if the current value matches the provided old value.
-- Returns the value of the element before the operation. Implies a
-- full memory barrier.
--
-- Note that lifted values in GHC have limited guarantees concerning
-- pointer equality. In particular, data constructor applications of
-- single-constructor data types may be mangled by GHC Core optimizations.
-- Users of this function are expected to understand how to make
-- pointer equality survive GHC's optimization passes.
casArray :: PrimMonad m
  => MutableArray (PrimState m) a -- ^ prim array
  -> Int -- ^ index
  -> a -- ^ expected old value
  -> a -- ^ new value
  -> m (Bool,a)
{-# INLINE casArray #-}
casArray (MutableArray arr#) (I# i#) old new =
  primitive $ \s0 -> case casArray# arr# i# old new s0 of
    (# s1, n, r #) -> (# s1, (isTrue# (n ==# 0# ),r) #)
