module HsForeign.String
  ( -- * CString
    mallocFromByteString
  , mallocFromMaybeByteString
  , withByteString
  , withByteStrings

    -- * CXX: std::string
  , StdString
  , hs_new_std_string
  , hs_new_std_string_def
  , hs_std_string_size
  , hs_std_string_cstr
  , hs_delete_std_string
  , unsafePeekStdString
  ) where

import           Control.Exception        (AssertionFailed (..), throw)
import           Control.Monad            (unless)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import           Data.Word
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal
import           Foreign.Ptr

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

withByteString :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withByteString (BS.PS fp off len) f =
  -- TODO: since bytestring 0.11.0.0, it exports the 'BS' constructor.
  -- we can change to benefit from the simplified BS constructor if we only
  -- support bytestring >= 0.11
  let fp' = fp `plusForeignPtr` off
   in withForeignPtr fp' $ \p -> f p len

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

-------------------------------------------------------------------------------
-- std::string

data StdString

foreign import ccall unsafe hs_new_std_string :: Ptr Word8 -> Int -> IO (Ptr StdString)
foreign import ccall unsafe hs_new_std_string_def :: IO (Ptr StdString)
foreign import ccall unsafe hs_std_string_size :: Ptr StdString -> IO Int
foreign import ccall unsafe hs_std_string_cstr :: Ptr StdString -> IO (Ptr Word8)
foreign import ccall unsafe hs_delete_std_string :: Ptr StdString -> IO ()

unsafePeekStdString :: Ptr StdString -> IO ByteString
unsafePeekStdString stdstring = do
  siz <- hs_std_string_size stdstring
  ptr <- hs_std_string_cstr stdstring
  BS.unsafePackCStringFinalizer ptr siz (hs_delete_std_string stdstring)
{-# INLINE unsafePeekStdString #-}
