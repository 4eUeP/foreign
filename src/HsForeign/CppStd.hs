{-# LANGUAGE MagicHash #-}

module HsForeign.CppStd
  ( -- * StdString
    StdString
  , newStdString
  , newStdStringFromShort
  , maybeNewStdString
  , hs_new_std_string
  , hs_new_std_string_def
  , hs_std_string_size
  , hs_std_string_cstr
  , hs_delete_std_string
  , unsafePeekStdString
  , peekStdStringShort
    -- * StdVector
  , StdVector

  ) where

import           Data.ByteString        (ByteString)
import           Data.ByteString.Short  (ShortByteString)
import qualified Data.ByteString.Short  as BSS
import qualified Data.ByteString.Unsafe as BS
import           Data.Word
import           Foreign.Ptr

import           HsForeign.Primitive
import           HsForeign.String

-------------------------------------------------------------------------------
-- StdString

data StdString

-- | New a c++ std::string from proviced bytestring.
--
-- The memory should be deallocated using delete when no longer required.
newStdString :: ByteString -> IO (Ptr StdString)
newStdString bs = withByteString bs $ hs_new_std_string

newStdStringFromShort :: ShortByteString -> IO (Ptr StdString)
newStdStringFromShort sbs = withShortByteString sbs $ hs_new_std_string'

maybeNewStdString :: Maybe ByteString -> IO (Ptr StdString)
maybeNewStdString Nothing   = pure nullPtr
maybeNewStdString (Just bs) = newStdString bs

unsafePeekStdString :: Ptr StdString -> IO ByteString
unsafePeekStdString stdstring = do
  siz <- hs_std_string_size stdstring
  ptr <- hs_std_string_cstr stdstring
  BS.unsafePackCStringFinalizer ptr siz (hs_delete_std_string stdstring)
{-# INLINE unsafePeekStdString #-}

peekStdStringShort :: Ptr StdString -> IO ShortByteString
peekStdStringShort stdstring = do
  siz <- hs_std_string_size stdstring
  ptr <- hs_std_string_cstr stdstring
  BSS.packCStringLen (castPtr ptr, siz)

-- TODO
--withStdString :: (Ptr StdString -> IO a) -> IO (ByteString, a)
--withStdString = undefined

foreign import ccall unsafe hs_new_std_string :: Ptr Word8 -> Int -> IO (Ptr StdString)
foreign import ccall unsafe "hs_new_std_string"
  hs_new_std_string' :: ByteArray# -> Int -> IO (Ptr StdString)
foreign import ccall unsafe hs_new_std_string_def :: IO (Ptr StdString)
foreign import ccall unsafe hs_std_string_size :: Ptr StdString -> IO Int
foreign import ccall unsafe hs_std_string_cstr :: Ptr StdString -> IO (Ptr Word8)
foreign import ccall unsafe hs_delete_std_string :: Ptr StdString -> IO ()

-------------------------------------------------------------------------------
-- StdVector

data StdVector a

-- TODO

--peekStdVectorOfString :: Ptr (StdVector StdString) -> [ByteString]
--peekStdVectorOfString = undefined

-------------------------------------------------------------------------------
-- StdMap

-- TODO

-------------------------------------------------------------------------------
-- StdMultiMap

-- TODO

--data CStdMultiMap k v
--newtype StdMultiMap k v = StdMultiMap (CStdMultiMap k v)
