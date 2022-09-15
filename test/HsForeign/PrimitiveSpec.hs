{-# LANGUAGE MagicHash #-}

module HsForeign.PrimitiveSpec (spec) where

import           System.IO.Unsafe      (unsafeDupablePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           HsForeign.Primitive

spec :: Spec
spec = describe "pass prim array list to foreign" $ do
  prop "sum first should be equal(unsafe FFI)" $ \xss ->
    let pas = map (primArrayFromList . getNonEmpty) xss :: [PrimArray Int]
        s = sum (map (head . getNonEmpty) xss)
     in unsafeDupablePerformIO (withPrimArrayListUnsafe pas sum_first_unsafe) === s

  prop "sum first should be equal(safe FFI)" $ \xss ->
    let pas = map (primArrayFromList . getNonEmpty) xss :: [PrimArray Int]
        s = sum (map (head . getNonEmpty) xss)
     in unsafeDupablePerformIO (withPrimArrayList pas sum_first_safe) === s

foreign import ccall unsafe sum_first_unsafe :: BAArray# Int -> Int -> IO Int
foreign import ccall safe sum_first_safe :: Ptr (Ptr Int) -> Int -> IO Int
