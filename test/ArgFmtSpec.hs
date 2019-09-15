module ArgFmtSpec ( spec ) where


import           Data.Maybe
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Text.Format


spec :: Spec
spec = describe "read" $ do
  prop "all" $ \(TestArgFmt (cs, fmt)) -> read cs == fmt


newtype TestArgFmt = TestArgFmt (String, ArgFmt) deriving Show

instance Arbitrary TestArgFmt where
  arbitrary = do
      (pad, align) <- genAlign
      sign <- genSign
      alternate <- genAlternate
      signAware <- genSignAware
      width <- genWidth
      sep <- genNumSep
      precision <- genPrecision
      specs <- genSpecs

      let cs1 = [x | (Just x) <- [ fst pad, fst align, fst sign, fst alternate
                                 , fst signAware
                                 ]
                ]
          cs2 = fromMaybe "" $ fst width
          cs3 = fromMaybe "" $ sequence [fst sep]
          cs4 = fromMaybe "" $ fst precision
          cs = concat [cs1, cs2, cs3, cs4, specs]
          fmt = ArgFmt (snd align) (snd pad) (snd sign) (snd alternate)
                       (snd signAware) (snd width) (snd sep) (snd precision)
                       specs
      return $ TestArgFmt (cs, fmt)
    where
      genAlign :: Gen ((Maybe Char, Char), (Maybe Char, FmtAlign))
      genAlign = do
        align <- elements [ (Nothing, AlignNone), (Just '<', AlignLeft)
                          , (Just '>', AlignRight), (Just '^', AlignCenter)
                          , (Just '=', AlignSign)
                          ]
        c <- maybe (return Nothing) (\_ -> arbitrary) $ fst align
        return (maybe (Nothing, ' ') (\c -> (Just c, c)) c, align)

      genPad :: Gen (Maybe Char, Char)
      genPad = do

        oneof $ [ return (Nothing, ' ')
                       , (\c -> (Just c, c)) <$> arbitrary
                       ]

      genSign :: Gen (Maybe Char, FmtSign)
      genSign = elements [ (Nothing, SignNone), (Just '+', SignPlus)
                         , (Just '-', SignMinus), (Just ' ', SignSpace)
                         ]

      genAlternate :: Gen (Maybe Char, Bool)
      genAlternate = elements [(Nothing, False), (Just '#', True)]

      genSignAware :: Gen (Maybe Char, Bool)
      genSignAware = elements [(Nothing, False), (Just '0', True)]

      genWidth :: Gen (Maybe String, Either Int ArgKey)
      genWidth = oneof [genEmptyWP, genLeftWP False, genRightWP False]

      genNumSep :: Gen (Maybe Char, FmtNumSep)
      genNumSep = elements [ (Nothing, NumSepNone), (Just '_', NumSepDash)
                           , (Just ',', NumSepComma)
                           ]

      genPrecision :: Gen (Maybe String, Either Int ArgKey)
      genPrecision = oneof [genEmptyWP, genLeftWP True, genRightWP True]

      genEmptyWP :: Gen (Maybe String, Either Int ArgKey)
      genEmptyWP = return (Nothing, Left (-1))

      genLeftWP :: Bool -> Gen (Maybe String, Either Int ArgKey)
      genLeftWP dot = do
        Positive x <- arbitrary
        let cs = if dot then ('.' : show x) else show x
        return (Just cs, Left x)

      genRightWP :: Bool -> Gen (Maybe String, Either Int ArgKey)
      genRightWP dot = do
        cs <- ('x' :) <$> suchThat arbitrary (all (not . (flip elem "!{}")))
        let cs1 = (if dot then ".{" else "{") ++ cs ++ "}"
        return (Just cs1, Right $ read cs)

      genSpecs :: Gen String
      genSpecs = elements [ "", "s"
                          , "b", "c", "d", "o", "x", "X"
                          , "e", "E", "f", "F", "g", "G", "%"
                          , "%Y-%m-%d"
                          ]
