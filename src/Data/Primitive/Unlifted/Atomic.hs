{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}

module Data.Primitive.Unlifted.Atomic
  ( casUnliftedArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState,primitive)
import Data.Primitive.Unlifted.Array (MutableUnliftedArray(..))
import Data.Primitive.Unlifted.Class (PrimUnlifted,Unlifted,toUnlifted#,fromUnlifted#)
import GHC.Exts (Any,MutableArrayArray#,MutableArray#,ArrayArray#,Int(I#))
import GHC.Exts (casArray#,isTrue#,(==#),unsafeCoerce#)

-- | Given an array, an offset, the expected old value,
-- and the new value, perform an atomic compare and swap i.e. write
-- the new value if the current value matches the provided old value.
-- Returns the value of the element before the operation. Implies a
-- full memory barrier.
--
-- Some unlifted types, in particular the ones that correspond to mutable
-- resources, have good guarantees about pointer equality. With these
-- types, this function is much easier to reason about than @casArray@.
casUnliftedArray :: forall m a. (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ array
  -> Int -- ^ index
  -> a -- ^ expected old value
  -> a -- ^ new value
  -> m (Bool,a)
{-# INLINE casUnliftedArray #-}
casUnliftedArray (MutableUnliftedArray arr#) (I# i#) old new =
  -- All of this unsafeCoercing is really nasty business. This will go away
  -- once https://github.com/ghc-proposals/ghc-proposals/pull/203 happens.
  -- Also, this is unsound if the result is immidiately consumed by
  -- the FFI.
  primitive $ \s0 ->
    let !uold = (unsafeCoerce# :: Unlifted a -> Any) (toUnlifted# old)
        !unew = (unsafeCoerce# :: Unlifted a -> Any) (toUnlifted# new)
     in case casArray# ((unsafeCoerce# :: MutableArrayArray# (PrimState m) -> MutableArray# (PrimState m) Any) arr#) i# uold unew s0 of
          (# s1, n, ur #) -> (# s1, (isTrue# (n ==# 0# ),fromUnlifted# ((unsafeCoerce# :: Any -> Unlifted a) ur)) #)

