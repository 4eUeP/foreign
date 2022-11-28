module HsForeign.StringSpec (spec) where

import           System.IO.Unsafe                     (unsafeDupablePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

import           HsForeign.CppStd
import           HsForeign.String

spec :: Spec
spec = do
  describe "StdString" $ do
    prop "unsafePeekStdString" $ \s ->
      -- 1. withByteString does nothing but pass the underlying ptr
      -- 2. hs_new_std_string copy construct a new std::string
      -- 3. unsafePeekStdString peek the std::string with a delete finalizer.
      let f = unsafePeekStdString =<< (withByteString s $ \p l -> hs_new_std_string p l)
       in unsafeDupablePerformIO f === s
