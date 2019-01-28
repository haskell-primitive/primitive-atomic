{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Primitive.PrimArray.Atomic
  ( casPrimArray
  , fetchAddPrimArray
  , fetchSubPrimArray
  , fetchAndPrimArray
  , fetchNandPrimArray
  , fetchOrPrimArray
  , fetchXorPrimArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState,primitive)
import Data.Primitive (MutablePrimArray(..))
import Data.Primitive.Class.Atomic (PrimMach,primMachToInt#,primMachFromInt#)
import GHC.Exts

-- | Given an array, an offset in Int units, the expected old value, and the new value,
-- perform an atomic compare and swap i.e. write the new value if the current value matches
-- the provided old value. Returns the value of the element before the operation. Implies
-- a full memory barrier.
casPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) a -- ^ prim array
  -> Int -- ^ index
  -> a -- ^ expected old value
  -> a -- ^ new value
  -> m a
{-# INLINE casPrimArray #-}
casPrimArray (MutablePrimArray arr#) (I# i#) old new =
  primitive $ \s0 -> case casIntArray# arr# i# (primMachToInt# old) (primMachToInt# new) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to add, atomically
-- add the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchAddPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) Int
  -> Int -- ^ index
  -> a -- ^ value to add to the element
  -> m a
{-# INLINE fetchAddPrimArray #-}
fetchAddPrimArray (MutablePrimArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchAddIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to subtract, atomically
-- subtract the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchSubPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) Int
  -> Int -- ^ index
  -> a -- ^ value to subtract from the element
  -> m a
{-# INLINE fetchSubPrimArray #-}
fetchSubPrimArray (MutablePrimArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchSubIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @AND@, atomically
-- @AND@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchAndPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) Int
  -> Int -- ^ index
  -> a -- ^ value to @AND@ with the element
  -> m a
{-# INLINE fetchAndPrimArray #-}
fetchAndPrimArray (MutablePrimArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchAndIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @NAND@, atomically
-- @NAND@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchNandPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) Int
  -> Int -- ^ index
  -> a -- ^ value to @NAND@ with the element
  -> m a
{-# INLINE fetchNandPrimArray #-}
fetchNandPrimArray (MutablePrimArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchNandIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @OR@, atomically
-- @OR@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchOrPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) Int
  -> Int -- ^ index
  -> a -- ^ value to @OR@ with the element
  -> m a
{-# INLINE fetchOrPrimArray #-}
fetchOrPrimArray (MutablePrimArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchOrIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @XOR@, atomically
-- @XOR@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchXorPrimArray :: (PrimMonad m, PrimMach a)
  => MutablePrimArray (PrimState m) Int
  -> Int -- ^ index
  -> a -- ^ value to @XOR@ with the element
  -> m a
{-# INLINE fetchXorPrimArray #-}
fetchXorPrimArray (MutablePrimArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchXorIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

