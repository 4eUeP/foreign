module HsForeign.AsyncFFISpec (spec) where

import           Foreign.StablePtr
import           GHC.Conc
import           System.IO.Unsafe      (unsafeDupablePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           HsForeign.AsyncFFI
import           HsForeign.Primitive

spec :: Spec
spec = describe "pass prim array list to async foreign function" $ do
  prop "sum first should be equal" $ \xss ->
    let pas = map (primArrayFromList . getNonEmpty) xss :: [PrimArray Int]
        s = sum (map (head . getNonEmpty) xss)
        f = withPrimArrayList pas $ \baa l -> withPrimAsyncFFI (async_sum_first baa l)
     in unsafeDupablePerformIO f === s

foreign import ccall unsafe async_sum_first
  :: Ptr (Ptr Int) -> Int
  -> StablePtr PrimMVar -> Int -> Ptr Int -> IO ()
