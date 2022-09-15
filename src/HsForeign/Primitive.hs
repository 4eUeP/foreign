{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

module HsForeign.Primitive
  ( BA# (BA#)
  , MBA# (MBA#)
  , BAArray# (BAArray#)
  , withPrim, allocPrim
  , withPrimUnsafe
  , allocPrimUnsafe
  , withPrimArray
  , allocPrimArray
  , withPrimArrayUnsafe
  , allocPrimArrayUnsafe
  , withPrimArrayList
  , withPrimArrayListUnsafe
  , withForeignPtrList

    -- * Internal helpers
  , withMutablePrimArrayContents
  , withPrimArrayContents
  , byteArrayContents#
  , mutableByteArrayContents#

    -- * Re-export
  , module Data.Primitive
  , module Control.Monad.Primitive
  ) where

import           Control.Monad                 (foldM_)
import           Control.Monad.Primitive
import           Data.Primitive
import           Data.Primitive.Unlifted.Array
import           Foreign.ForeignPtr
import           GHC.Exts

-------------------------------------------------------------------------------

newtype BA# a = BA# ByteArray#
newtype MBA# a = MBA# (MutableByteArray# RealWorld)
newtype BAArray# a = BAArray# ArrayArray#

-- From Z-Data package: Z.Foreign
--
-- | Create an one element primitive array and use it as a pointer to the
-- primitive element.
--
-- Don't pass a forever loop to this function,
-- see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrim :: forall a b. Prim a => a -> (Ptr a -> IO b) -> IO (a, b)
withPrim v f = do
  buf <- newAlignedPinnedPrimArray 1
  writePrimArray buf 0 v
  !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
  !a <- readPrimArray buf 0
  return (a, b)
{-# INLINABLE withPrim #-}

-- From Z-Data package: Z.Foreign
--
-- | like 'withPrim', but don't write initial value.
allocPrim :: forall a b. Prim a => (Ptr a -> IO b) -> IO (a, b)
allocPrim f = do
  buf <- newAlignedPinnedPrimArray 1
  !b <- withMutablePrimArrayContents buf $ \ ptr -> f ptr
  !a <- readPrimArray buf 0
  return (a, b)
{-# INLINABLE allocPrim #-}

-- From Z-Data package: Z.Foreign
--
-- | Create an one element primitive array and use it as a pointer to the
-- primitive element.
--
-- Return the element and the computation result.
--
-- USE THIS FUNCTION WITH UNSAFE SYNC FFI CALL ONLY.
withPrimUnsafe :: (Prim a) => a -> (MBA# a -> IO b) -> IO (a, b)
withPrimUnsafe v f = do
  -- All heap objects are WORD aligned so no need to do extra alignment
  mpa@(MutablePrimArray mba#) <- newPrimArray 1
  writePrimArray mpa 0 v
  !b <- f (MBA# mba#)
  !a <- readPrimArray mpa 0
  return (a, b)
{-# INLINE withPrimUnsafe #-}

-- From Z-Data package: Z.Foreign
--
-- | like 'withPrimUnsafe', but don't write initial value.
--
-- USE THIS FUNCTION WITH UNSAFE SYNC FFI CALL ONLY.
allocPrimUnsafe :: (Prim a) => (MBA# a -> IO b) -> IO (a, b)
allocPrimUnsafe f = do
  -- All heap objects are WORD aligned so no need to do extra alignment
  mpa@(MutablePrimArray mba#) <- newPrimArray 1
  !b <- f (MBA# mba#)
  !a <- readPrimArray mpa 0
  return (a, b)
{-# INLINE allocPrimUnsafe #-}

-- From Z-Data package: Z.Foreign
--
-- | Pass primitive array to safe FFI as pointer.
--
-- Use proper pointer type and @HsInt@ to marshall @Ptr a@ and @Int@ arguments
-- on C side.
-- The memory pointed by 'Ptr a' will not moved during call. After call returned,
-- pointer is no longer valid.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- Don't pass a forever loop to this function,
-- see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArray :: (Prim a) => PrimArray a -> (Ptr a -> Int -> IO b) -> IO b
withPrimArray arr f
  | isPrimArrayPinned arr = do
      let siz = sizeofPrimArray arr
      withPrimArrayContents arr $ \ptr -> f ptr siz
  | otherwise = do
      let siz = sizeofPrimArray arr
      buf <- newPinnedPrimArray siz
      copyPrimArray buf 0 arr 0 siz
      withMutablePrimArrayContents buf $ \ptr -> f ptr siz
{-# INLINABLE withPrimArray #-}

-- From Z-Data package: Z.Foreign
--
-- | Allocate a prim array and pass to FFI as pointer, freeze result into a 'PrimVector'.
allocPrimArray :: forall a b . Prim a
               => Int      -- ^ in elements
               -> (Ptr a -> IO b)
               -> IO (PrimArray a, b)
allocPrimArray len f = do
  mpa <- newAlignedPinnedPrimArray len
  !r <- withMutablePrimArrayContents mpa f
  !pa <- unsafeFreezePrimArray mpa
  return (pa, r)
{-# INLINABLE allocPrimArray #-}

-- From Z-Data package: Z.Foreign
--
-- | Pass primitive array to unsafe FFI as pointer.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use proper pointer
-- type and @HsInt@ to marshall @ByteArray#@ and @Int@ arguments on C side.
--
-- The second 'Int' arguement is the element size not the bytes size.
--
-- USE THIS FUNCTION WITH UNSAFE SYNC FFI CALL ONLY.
withPrimArrayUnsafe :: (Prim a) => PrimArray a -> (BA# a -> Int -> IO b) -> IO b
withPrimArrayUnsafe pa@(PrimArray ba#) f = f (BA# ba#) (sizeofPrimArray pa)
{-# INLINE withPrimArrayUnsafe #-}

-- From Z-Data package: Z.Foreign
--
-- | Allocate some bytes and pass to FFI as pointer, freeze result into a
-- 'PrimArray'.
--
-- USE THIS FUNCTION WITH UNSAFE SYNC FFI CALL ONLY.
allocPrimArrayUnsafe
  :: forall a b. Prim a => Int -> (MBA# a -> IO b) -> IO (PrimArray a, b)
allocPrimArrayUnsafe len f = do
  (mpa@(MutablePrimArray mba#) :: MutablePrimArray RealWorld a) <- newPrimArray len
  !r <- f (MBA# mba#)
  !pa <- unsafeFreezePrimArray mpa
  return (pa, r)
{-# INLINE allocPrimArrayUnsafe #-}

-- From Z-Data package: Z.Foreign
--
-- | Pass primitive array list to safe FFI as pointer.
--
-- Use proper pointer type and @HsInt@ to marshall @Ptr (Ptr a)@ and @Int@
-- arguments on C side.
-- The memory pointed by 'Ptr a' will not moved during call. After call returned,
-- pointer is no longer valid.
--
-- The second 'Int' arguement is the list size.
--
-- Don't pass a forever loop to this function,
-- see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArrayList
  :: Prim a => [PrimArray a] -> (Ptr (Ptr a) -> Int -> IO b) -> IO b
withPrimArrayList pas0 f = do
  let l = length pas0
  ptrs <- newPinnedPrimArray l
  go ptrs 0 pas0
  where
    go ptrs !_ [] = do pa <- unsafeFreezePrimArray ptrs
                       withPrimArray pa f
    go ptrs !i (pa:pas) =
      -- It's important to nest 'withPrimArray' calls to keep all pointers alive
      withPrimArray pa $ \ ppa _ -> do
        writePrimArray ptrs i ppa
        go ptrs (i+1) pas

withForeignPtrList :: [ForeignPtr a] -> (Ptr (Ptr a) -> Int -> IO b) -> IO b
withForeignPtrList fptrs f = do
  let l = length fptrs
  ptrs <- newPinnedPrimArray l
  go ptrs 0 fptrs
  where
    go ptrs !_ [] = do
      pa <- unsafeFreezePrimArray ptrs
      withPrimArray pa f
    go ptrs !i (fp:fps) = do
      withForeignPtr fp $ \p -> do
        writePrimArray ptrs i p
        go ptrs (i+1) fps

-- From Z-Data package: Z.Foreign, with slight modification.
--
-- | Pass primitive array list to unsafe FFI as @StgArrBytes**@.
--
-- Enable 'UnliftedFFITypes' extension in your haskell code, use
-- @StgArrBytes**@(>=8.10) or @StgMutArrPtrs*@(<8.10) pointer type and @HsInt@
-- to marshall @BAArray#@ and @Int@ arguments on C side.
--
-- The second 'Int' arguement is the list size.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
withPrimArrayListUnsafe :: [PrimArray a] -> (BAArray# a -> Int -> IO b) -> IO b
withPrimArrayListUnsafe pas f = do
  let l = length pas
  mla <- unsafeNewUnliftedArray l
  foldM_ (\ !i pa -> writeUnliftedArray mla i pa >> return (i + 1)) 0 pas
  (UnliftedArray la#) <- unsafeFreezeUnliftedArray mla
  f (BAArray# (unsafeCoerce# la#)) l
{-# INLINE withPrimArrayListUnsafe #-}

-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 902
-- ghc<9.2 does not has a 'mutableByteArrayContents#'
mutableByteArrayContents# :: MutableByteArray# s -> Addr#
mutableByteArrayContents# mba# = byteArrayContents# (unsafeCoerce# mba#)
{-# INLINE mutableByteArrayContents# #-}
#endif

-- From Z-Data package
--
-- | Obtain the pointer to the content of an mutable array, and the pointer
-- should only be used during the IO action.
--
-- This operation is only safe on /pinned/ primitive arrays (Arrays allocated
-- by 'newPinnedPrimArray' or 'newAlignedPinnedPrimArray').
--
-- Don't pass a forever loop to this function,
-- see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withMutablePrimArrayContents
  :: MutablePrimArray RealWorld a
  -> (Ptr a -> IO b)
  -> IO b
withMutablePrimArrayContents (MutablePrimArray mba#) f = do
    let addr# = mutableByteArrayContents# mba#
        ptr = Ptr addr#
    b <- f ptr
    primitive_ (touch# mba#)
    return b
{-# INLINE withMutablePrimArrayContents #-}

-- From Z-Data package
--
-- | Obtain the pointer to the content of an array, and the pointer should only
-- be used during the IO action.
--
-- This operation is only safe on /pinned/ primitive arrays (Arrays allocated
-- by 'newPinnedPrimArray' or 'newAlignedPinnedPrimArray').
--
-- Don't pass a forever loop to this function,
-- see <https://ghc.haskell.org/trac/ghc/ticket/14346 #14346>.
withPrimArrayContents :: PrimArray a -> (Ptr a -> IO b) -> IO b
withPrimArrayContents (PrimArray ba#) f = do
    let addr# = byteArrayContents# ba#
        ptr = Ptr addr#
    b <- f ptr
    primitive_ (touch# ba#)
    return b
{-# INLINE withPrimArrayContents #-}
