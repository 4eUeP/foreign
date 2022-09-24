module HsForeign.Utils
  ( withMaybePtr
  ) where

import           Foreign.Ptr

withMaybePtr
  :: Maybe a
  -> (a -> (Ptr a -> IO b) -> IO b)
  -> (Ptr a -> IO b)
  -> IO b
withMaybePtr Nothing _withx f = f nullPtr
withMaybePtr (Just x) withx f = withx x f
{-# INLINABLE withMaybePtr #-}
