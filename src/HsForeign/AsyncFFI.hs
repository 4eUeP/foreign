module HsForeign.AsyncFFI
  ( withAsyncFFI
  , withAsyncFFI'
  , withPrimAsyncFFI
  ) where

import           Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import           Control.Exception       (mask_, onException)
import           Control.Monad           (void)
import           Foreign.ForeignPtr
import           Foreign.StablePtr
import           GHC.Conc

import           HsForeign.Primitive

withAsyncFFI
  :: Int
  -- ^ Size of callback data
  -> (Ptr a -> IO a)
  -- ^ Peek callback data
  -> (StablePtr PrimMVar -> Int -> Ptr a -> IO b)
  -- ^ Normal async foreign function: sp -> cap -> ptr -> ()
  -> IO a
withAsyncFFI = withAsyncFFI' []

withPrimAsyncFFI
  :: Prim a
  => (StablePtr PrimMVar -> Int -> Ptr a -> IO b)
  -> IO a
withPrimAsyncFFI f = mask_ $ do
  (ret, _) <- allocPrim $ \ret' -> do
    mvar <- newEmptyMVar
    sp <- newStablePtrPrimMVar mvar
    (cap, _) <- threadCapability =<< myThreadId
    void $ f sp cap ret'
    takeMVar mvar `onException` forkIO (do takeMVar mvar;)
  pure ret
{-# INLINABLE withPrimAsyncFFI #-}

-- NOTE: memory allocated by haskell and pass to async cpp function must be
-- pinned.
withAsyncFFI'
  :: [MutableByteArray RealWorld]
  -- ^ A list of extra Bytes we will touch, usually it's empty
  -> Int
  -- ^ Size of callback data
  -> (Ptr a -> IO a)
  -- ^ Peek callback data
  -> (StablePtr PrimMVar -> Int -> Ptr a -> IO b)
  -- ^ Normal async foreign function: sp -> cap -> ptr -> ()
  -> IO a
withAsyncFFI' bas size peek_fun f = mask_ $ do
  mvar <- newEmptyMVar
  sp <- newStablePtrPrimMVar mvar
  fp <- mallocForeignPtrBytes size
  withForeignPtr fp $ \data' -> do
    (cap, _) <- threadCapability =<< myThreadId
    void $ f sp cap data'
    takeMVar mvar `onException` forkIO (do takeMVar mvar; touchForeignPtr fp; touch bas)
    peek_fun data'
{-# INLINABLE withAsyncFFI' #-}
