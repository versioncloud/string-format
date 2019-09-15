module ArgKeySpec ( spec ) where


import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Text.Format


spec :: Spec
spec = describe "read" $ do
  prop "empty" $ read "" == Index (-1)
  prop "index" $ \(NonNegative x) -> read (show x) == Index x
  prop "name"  $ \(NoSep cs) -> read cs == Name cs
  prop "nest"  $ \(OneSep (cs, cs1, cs2)) ->
    read cs == Nest (read cs1) (read cs2)
  prop "deep nest"  $ \(TwoSep (cs, cs1, cs2, cs3)) ->
    read cs == Nest (read cs1) (Nest (read cs2) (read cs3))
  prop "escaped" $ \(Escaped (cs, cs0)) -> read cs == Name cs0


newtype NoSep = NoSep String deriving Show

instance Arbitrary NoSep where
  arbitrary = do
    cs <- suchThat arbitrary (all (/= '!') . getNonEmpty)
    return $ NoSep $ 'x' : getNonEmpty cs


newtype OneSep = OneSep (String, String, String) deriving Show

instance Arbitrary OneSep where
  arbitrary = do
    (NoSep cs1) <- arbitrary
    (NoSep cs2) <- arbitrary
    return $ OneSep (cs1 ++ ('!' : cs2), cs1, cs2)


newtype TwoSep = TwoSep (String, String, String, String) deriving Show

instance Arbitrary TwoSep where
  arbitrary = do
    (NoSep cs1) <- arbitrary
    (NoSep cs2) <- arbitrary
    (NoSep cs3) <- arbitrary
    return $ TwoSep (cs1 ++ ('!' : cs2) ++ ('!' : cs3), cs1, cs2, cs3)


newtype Escaped = Escaped (String, String) deriving Show

instance Arbitrary Escaped where
  arbitrary = do
    (NoSep cs1) <- arbitrary
    (NoSep cs2) <- arbitrary
    return $ Escaped (cs1 ++ "!!" ++ cs2, cs1 ++ "!" ++ cs2)
