{-# LANGUAGE OverloadedStrings #-}

module FormatSpec ( spec ) where

import           Data.Char
import           Data.String
import           Numeric
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Text.Format


spec :: Spec
spec = basicSpec >> alignSpec >> alternateSpec >> otherSpec


basicSpec :: Spec
basicSpec = describe "basic" $ do
  prop "string" $ \cs -> format "{0:s}" cs == (cs :: String)
  prop "integer" $ \i -> format "{0:d}" i == show (i :: Integer)
  prop "float" $ \i -> format "{0:f}" i == showFFloat (Just 6) (i :: Float) ""
  prop "auto index 1" $ \cs -> format "{}" cs == (cs :: String)
  prop "auto index 2" $ \(cs, i) ->
    format "{} {}" cs i == cs ++ " " ++ show (i :: Integer)


alignSpec :: Spec
alignSpec = describe "align" $ do
  prop "left" $ \cs ->
    let result = cs ++ replicate (30 - length cs) ' '
    in  format "{:<30}" cs == result
  prop "right" $ \cs ->
    let result = replicate (30 - length cs) ' ' ++ cs
    in  format "{:>30}" cs == result
  prop "center" $ \cs ->
    let n = 30 - length cs
        ln = div n 2
        result = replicate ln ' ' ++ cs ++ replicate (n - ln) ' '
    in  format "{:^30}" cs == result
  prop "sign" $ \i ->
    let is = show (i :: Integer)
        sign = if i < 0 then head is else '+'
        digits = if i < 0 then tail is else is
        result = sign : (replicate (29 - length digits) ' ' ++ digits)
     in format "{:=+30}" i == result
  prop "pad" $ \(PadChar c, cs) ->
    let fmt = fromString $ "{:" ++ (c : "<30}")
        result = cs ++ replicate (30 - length cs) c
    in format fmt cs == result


signSpec :: Spec
signSpec = describe "sign" $ do
  prop "plus" $ \i ->
    let fn = if i < 0 then id else ('+' :)
    in format "{:+d}" i == (fn $ show (i :: Integer))
  prop "minus" $ \i -> format "{:-d}" i == show (i :: Integer)
  prop "space" $ \i ->
    let fn = if i < 0 then id else (' ' :)
    in format "{: d}" i == fn (show (i :: Integer))


alternateSpec :: Spec
alternateSpec = describe "alternate" $ do
  prop "binary" $ \i ->
    let fn = (if i < 0 then ('-' :) else id) . ("0b" ++)
        i1 = abs i
    in format "{:#b}" i == fn (showIntAtBase 2 intToDigit (i1 :: Integer) "")
  prop "octal" $ \i ->
    let fn = (if i < 0 then ('-' :) else id) . ("0o" ++)
        i1 = abs i
    in format "{:#o}" i == fn (showIntAtBase 8 intToDigit (i1 :: Integer) "")
  prop "hex" $ \i ->
    let fn = (if i < 0 then ('-' :) else id) . ("0x" ++)
        i1 = abs i
    in format "{:#x}" i == fn (showIntAtBase 16 intToDigit (i1 :: Integer) "")
  prop "HEX" $ \i ->
    let fn = (if i < 0 then ('-' :) else id) . ("0X" ++) . (map toUpper)
        i1 = abs i
    in format "{:#X}" i == fn (showIntAtBase 16 intToDigit (i1 :: Integer) "")


otherSpec :: Spec
otherSpec = describe "others" $ do
  prop "sign aware" $ \i ->
    let fn = if i < 0 then ('-' :) else ('+' :)
        is = show $ abs (i :: Integer)
    in format "{:+030d}" i == fn (replicate (29 - length is) '0' ++ is)
  prop "min width" $ \cs ->
    length (format "{:30s}" cs :: String) == max 30 (length (cs :: String))
  prop "max width" $ \cs ->
    length (format "{:.30s}" cs :: String) == min 30 (length (cs :: String))
  prop "number separation" $ \i ->
    let fn = \cs -> if length cs > 3 then (head $ drop 3 cs) : (fn $ drop 4 cs)
                                     else []
    in all (== '_') $ fn (format "{:_d}" (i :: Integer))
  prop "precision" $ \(i, NonNegative p) ->
    let fmt = fromString $ "{:." ++ (show p) ++ "}"
    in format fmt i == showFFloat (Just p) (i :: Double) ""


newtype PadChar = PadChar Char deriving Show

instance Arbitrary PadChar where
  arbitrary = PadChar <$> suchThat arbitrary (not . flip elem ['{', '}'])
