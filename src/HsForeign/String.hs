{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}

module HsForeign.String
  ( mallocFromByteString
  , mallocFromMaybeByteString
  , newStablePtrByteString
  , withByteString
  , withMaybeByteString
  , withByteStringList
  , withByteStrings
  , withShortByteString
  ) where

import           Control.Exception              (AssertionFailed (..), throw)
import           Control.Monad                  (unless)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Internal       as BS
import           Data.ByteString.Short          (ShortByteString)
import qualified Data.ByteString.Short.Internal as BSS
import qualified Data.ByteString.Unsafe         as BS
import           Data.Word
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.ForeignPtr.Unsafe
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.StablePtr
import           GHC.Exts                       (touch#)

import           HsForeign.Primitive

-------------------------------------------------------------------------------
-- CString

-- | Copies the content of the given ByteString.
--
-- The memory may be deallocated using free or finalizerFree when no longer
-- required.
mallocFromByteString :: ByteString -> IO (CString, Int)
mallocFromByteString bs =
  BS.unsafeUseAsCStringLen bs $ \(src, len) -> do
    buf <- mallocBytes len
    copyBytes buf src len
    return (buf, len)
{-# INLINE mallocFromByteString #-}

mallocFromMaybeByteString :: Maybe ByteString -> IO (CString, Int)
mallocFromMaybeByteString (Just bs) = mallocFromByteString bs
mallocFromMaybeByteString Nothing   = return (nullPtr, 0)
{-# INLINE mallocFromMaybeByteString #-}

newStablePtrByteString :: ByteString -> IO (CString, Int, StablePtr ByteString)
newStablePtrByteString bs = do
  !sp <- newStablePtr bs
  let (fp, len) = toForeignPtr0 bs
      !p = unsafeForeignPtrToPtr fp
  pure (castPtr p, len, sp)

withByteString :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withByteString bs f =
  let (fp, len) = toForeignPtr0 bs
   in withForeignPtr fp $ \p -> f p len
{-# INLINABLE withByteString #-}

withMaybeByteString :: Maybe ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withMaybeByteString Nothing f = f nullPtr 0
withMaybeByteString (Just bs) f =
  let (fp, len) = toForeignPtr0 bs
   in withForeignPtr fp $ \p -> f p len
{-# INLINABLE withMaybeByteString #-}

-- | Pass list of ByteStrings to FFI.
withByteStringList
  :: [ByteString]
  -> (Ptr (Ptr Word8) -> Ptr Int -> Int -> IO a)
  -- ^ cstring*, len*, list_len
  -> IO a
withByteStringList bss f = do
  let (ps, lens) = unzip (map toForeignPtr0 bss)
  withPrimArray (primArrayFromList lens) $ \lens' num ->
    withForeignPtrList ps $ \ps' num_ps -> do
      unless (num == num_ps) $ throw $
        AssertionFailed "This should never happen..."
      f ps' lens' num

{-# DEPRECATED withByteStrings "use withByteStringList instead" #-}
-- | Pass list of ByteStrings to FFI.
withByteStrings
  :: [ByteString]
  -> (Ptr (Ptr Word8) -> Ptr Int -> Ptr Int -> Int -> IO a)
  -- ^ cstring*, offset*, len*, list_len
  -> IO a
withByteStrings bss f = do
  let exbs (BS.PS payload off len) = (payload, fromIntegral off, fromIntegral len)
      (ps, offs, lens) = unzip3 (map exbs bss)
  withPrimArray (primArrayFromList lens) $ \lens' num ->
    withPrimArray (primArrayFromList offs) $ \offs' _num_offs ->
    withForeignPtrList ps $ \ps' _num_ps -> do
      unless (num == _num_offs && num == _num_ps) $ throw $
        AssertionFailed "This should never happen..."
      f ps' offs' lens' num

withShortByteString :: ShortByteString -> (ByteArray# -> Int -> IO a) -> IO a
withShortByteString sbs@(BSS.SBS ba#) f = do
  !r <- f ba# (BSS.length sbs)
  primitive_ $ touch# ba#
  pure r

-------------------------------------------------------------------------------

-- TODO: since bytestring 0.11.0.0, it exports the 'BS' constructor and
-- 'toForeignPtr0'.
--
-- we can change to benefit from the simplified BS constructor if we only
-- support bytestring >= 0.11
toForeignPtr0 :: ByteString -> (ForeignPtr Word8, Int)
#if !MIN_VERSION_bytestring(0, 11, 0)
toForeignPtr0 (BS.PS fp off len) = (fp `plusForeignPtr` off, len)
#else
toForeignPtr0 = BS.toForeignPtr0
#endif
