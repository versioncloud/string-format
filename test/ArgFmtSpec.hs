{-# LANGUAGE RecordWildCards #-}

module ArgFmtSpec ( spec ) where


import           Data.Maybe
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Text.Format


spec :: Spec
spec = describe "read" $ do
  prop "pretty" $ \(TestArgFmt (cs, fmt)) -> read cs == fmt


newtype TestArgFmt = TestArgFmt (String, ArgFmt) deriving Show

instance Arbitrary TestArgFmt where
  arbitrary = do
      (fmtPad, fmtAlign) <- genAlign
      fmtSign <- genSign
      fmtAlternate <- choose (True, False)
      fmtSignAware <- choose (True, False)
      fmtWidth <- genWidth
      fmtNumSep <- elements [ NumSepNone, NumSepDash, NumSepComma ]
      fmtPrecision <- genWidth
      fmtSpecs <- genSpecs

      let fmtRaw = ""
          fmt = ArgFmt{..}
          raw = prettyArgFmt fmt

      return $ TestArgFmt (raw, fmt{fmtRaw=raw})
    where
      genAlign :: Gen (Char, FmtAlign)
      genAlign = do
        align <- elements [ AlignNone, AlignLeft, AlignRight, AlignCenter
                          , AlignSign
                          ]
        pad <- if align == AlignNone then return ' '
                                     else arbitraryPrintableChar
        return (pad, align)

      genSign :: Gen FmtSign
      genSign = elements [ SignNone, SignPlus, SignMinus, SignSpace ]

      genWidth :: Gen (Either Int ArgKey)
      genWidth = oneof [ (Left . getNonNegative) <$> arbitrary
                       , Right <$> genKey
                       ]

      genKey :: Gen ArgKey
      genKey = do
        cs <- listOf $ suchThat arbitraryPrintableChar (not . (`elem` "!{}"))
        return $ read cs

      genSpecs :: Gen String
      genSpecs = elements [ "", "s"
                          , "b", "c", "d", "o", "x", "X"
                          , "e", "E", "f", "F", "g", "G", "%"
                          , "%Y-%m-%d"
                          ]
