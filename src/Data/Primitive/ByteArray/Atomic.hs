{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Data.Primitive.ByteArray.Atomic
  ( casByteArray
  , fetchAddByteArray
  , fetchSubByteArray
  , fetchAndByteArray
  , fetchNandByteArray
  , fetchOrByteArray
  , fetchXorByteArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState,primitive)
import Data.Primitive (MutableByteArray(..))
import Data.Primitive.Class.Atomic (PrimMach,primMachToInt#,primMachFromInt#)
import GHC.Exts

-- | Given an array, an offset in Int units, the expected old value, and the new value,
-- perform an atomic compare and swap i.e. write the new value if the current value matches
-- the provided old value. Returns the value of the element before the operation. Implies
-- a full memory barrier.
casByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m) -- ^ array
  -> Int -- ^ index
  -> a -- ^ expected old value
  -> a -- ^ new value
  -> m a
{-# INLINE casByteArray #-}
casByteArray (MutableByteArray arr#) (I# i#) old new =
  primitive $ \s0 -> case casIntArray# arr# i# (primMachToInt# old) (primMachToInt# new) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to add, atomically
-- add the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchAddByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ index
  -> a -- ^ value to add to the element
  -> m a
{-# INLINE fetchAddByteArray #-}
fetchAddByteArray (MutableByteArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchAddIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to subtract, atomically
-- subtract the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchSubByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ index
  -> a -- ^ value to subtract from the element
  -> m a
{-# INLINE fetchSubByteArray #-}
fetchSubByteArray (MutableByteArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchSubIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @AND@, atomically
-- @AND@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchAndByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ index
  -> a -- ^ value to @AND@ with the element
  -> m a
{-# INLINE fetchAndByteArray #-}
fetchAndByteArray (MutableByteArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchAndIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @NAND@, atomically
-- @NAND@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchNandByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ index
  -> a -- ^ value to @NAND@ with the element
  -> m a
{-# INLINE fetchNandByteArray #-}
fetchNandByteArray (MutableByteArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchNandIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @OR@, atomically
-- @OR@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchOrByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ index
  -> a -- ^ value to @OR@ with the element
  -> m a
{-# INLINE fetchOrByteArray #-}
fetchOrByteArray (MutableByteArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchOrIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)

-- | Given an array, and offset in Int units, and a value to @XOR@, atomically
-- @XOR@ the value to the element. Returns the value of the element before the
-- operation. Implies a full memory barrier.
fetchXorByteArray :: (PrimMonad m, PrimMach a)
  => MutableByteArray (PrimState m)
  -> Int -- ^ index
  -> a -- ^ value to @XOR@ with the element
  -> m a
{-# INLINE fetchXorByteArray #-}
fetchXorByteArray (MutableByteArray arr#) (I# i#) val =
  primitive $ \s0 -> case fetchXorIntArray# arr# i# (primMachToInt# val) s0 of
    (# s1, r #) -> (# s1, primMachFromInt# r #)


