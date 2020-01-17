{-# LANGUAGE RecordWildCards #-}

module Text.Format.ArgFmt
  ( FmtAlign(..)
  , FmtSign(..)
  , FmtNumSep(..)
  , ArgFmt(..)
  , prettyArgFmt
  , formatText
  , formatNumber
  ) where

import           Control.Arrow
import           Data.Char          (isDigit)
import qualified Data.List          as L
import           Numeric

import           Text.Format.ArgKey
import           Text.Format.Error


-- | How to align argument
--
-- Note: 'AlignNone' is equivalent to 'AlignLeft' unless
-- number's sign aware enabled
--
data FmtAlign = AlignNone     -- ^ alignment is not specified
              | AlignLeft     -- ^ pad chars before argument
              | AlignRight    -- ^ pad chars before argument
              | AlignCenter   -- ^ pad chars before and after argument
              | AlignSign     -- ^ number specified, pad between sign and digits
              deriving (Show, Eq)


-- | How to show number's sign
--
-- Note: 'SignNone' is equivalent to 'SignMinus' for signed numbers
data FmtSign = SignNone      -- ^ sign is not specified
             | SignPlus      -- ^ show \'+\' for positive and \'-\' for negative
             | SignMinus     -- ^ show negative's sign only
             | SignSpace     -- ^ show ' ' for positive and '-' for negative
             deriving (Show, Eq)


-- | Number separator
data FmtNumSep = NumSepNone   -- ^ don't seprate
               | NumSepDash   -- ^ seprate by '_'
               | NumSepComma  -- ^ seprate by ','
               deriving (Show, Eq)


-- | Description of argument format options
--
-- When read from string, the sytax is as follows:
--
-- > [[pad]align][sign][#][0][width][separator][.precision][specs]
--
-- * __[]__ means an optional field (or filed group)
-- * __pad__ means char to be used for padding, it should be a literal 'Char',
--   default is space
-- * __align__ means align option
--
--    @
--      <       AlignLeft
--      >       AlignRight
--      ^       AlignCenter
--      =       AlignSign
--      empty   AlignNone
--    @
--
--  * __sign__ means number sign option
--
--    @
--      +       SignPlus
--      -       SignMinus
--      space   SignSpace
--      empty   SignNone
--    @
--
--  * __#__ means number alternate form option
--
--  * __0__ preceding __width__ option means sign-aware as well as zero-padding
--
--    @
--      number       AlignNone & sign aware = AlignSign & pad '0'
--      other types  means nothing
--    @
--
--  * __width__ means minimum argument width,
--    it may be an 'ArgKey' indicates it's value from another integer argument
--
--    @
--      integer   minimum width
--      empty     no minimum widht constrain
--    @
--
--  * __separator__ number separator option
--
--    @
--      _       NumSepDash
--      ,       NumSepComma
--      empty   NumSepNone
--    @
--
--  * __precision__ (must leading with a dot)
--    number preceding or maximum with option
--    it may be an 'ArgKey' indicates it's value from another integer argument
--
--    @
--      for number (floating point) types   number precision
--      for non-number types                maximum widht
--    @
--
--  * __specs__ type specified options,
--    it determines how data should be presented,
--    see available type presentions below
--
--
--  == String presentions
--  @
--    s               explicitly specified string type
--    empty           implicitly specified string type
--  @
--
--  == Integer presentions
--  @
--    b               binary format integer
--    c               char point ('Char' will be trasformed by 'Char.ord' first)
--    d               decimal format integer
--    o               octal format integer
--    x               hex format integer (use lower-case letters)
--    X               hex format integer (use upper-case letters)
--    empty           same as "d"
--  @
--
--  == Floating point number presentions
--  @
--    e               exponent notation, see 'Numeric.showEFloat'
--    E               same as "e", but use upper-case 'E' as separator
--    f               fixed-point notation see 'Numeric.showFFloat'
--    F               same as "f", but converts nan to NAN and inf to INF
--    g               general format, see 'Numeric.showGFloat'
--    G               same as "g", but use upper-case 'E' as separator and
--                    converts nan to NAN and inf to INF
--    %               percentage, same as "f" except multiplies 100 first and
--                    followed by a percent sign
--    empty           same as "g"
--  @
--
--  == Examples
--  >>> read "*<30s" :: ArgFmt
--  >>> read "<10.20s" :: ArgFmt
--  >>> read "0=10_.20d" :: ArgFmt
--  >>> read "#010_.20b" :: ArgFmt
--
data ArgFmt = ArgFmt { fmtAlign     :: FmtAlign
                     , fmtPad       :: Char
                     , fmtSign      :: FmtSign
                     , fmtAlternate :: Bool
                     , fmtSignAware :: Bool
                     , fmtWidth     :: Either Int ArgKey
                     , fmtNumSep    :: FmtNumSep
                     , fmtPrecision :: Either Int ArgKey
                     , fmtSpecs     :: String
                     , fmtRaw       :: String
                     } deriving (Show, Eq)

instance Read ArgFmt where
  readsPrec _ cs =
      let (fmtAlign, fmtPad, cs1) = parseAlign cs
          (fmtSign, cs2) = parseSign cs1
          (fmtAlternate, cs3) = parseAlternate cs2
          (fmtSignAware, cs4) = parseSignAware cs3
          (fmtWidth, cs5) = parseWidth cs4
          (fmtNumSep, cs6) = parseNumSep cs5
          (fmtPrecision, fmtSpecs) = parsePrecision cs6
          fmtRaw = cs
      in [ (ArgFmt{..}, "") ]
    where
      stack :: String -> String
      stack = reverse . (`drop` (reverse cs)) . length

      parseAlign (c : '<' : cs) = (AlignLeft, c, cs)
      parseAlign (c : '>' : cs) = (AlignRight, c, cs)
      parseAlign (c : '^' : cs) = (AlignCenter, c, cs)
      parseAlign (c : '=' : cs) = (AlignSign, c, cs)
      parseAlign ('<' : cs)     = (AlignLeft, ' ', cs)
      parseAlign ('>' : cs)     = (AlignRight,' ', cs)
      parseAlign ('^' : cs)     = (AlignCenter, ' ', cs)
      parseAlign ('=' : cs)     = (AlignSign,' ', cs)
      parseAlign cs             = (AlignNone, ' ', cs)

      parseSign ('+' : cs) = (SignPlus, cs)
      parseSign ('-' : cs) = (SignMinus, cs)
      parseSign (' ' : cs) = (SignSpace, cs)
      parseSign cs         = (SignNone, cs)

      parseAlternate ('#' : cs) = (True, cs)
      parseAlternate cs         = (False, cs)

      parseSignAware ('0' : cs) = (True, cs)
      parseSignAware cs         = (False, cs)

      parseWidth ('{' : cs) =
        case L.break (== '}') cs of
          (ks, '}' : cs1) -> (Right (read ks), cs1)
          _               -> errorCloseTag $ stack cs
      parseWidth cs         =
        case L.break (not . isDigit) cs of
          ("", cs1) -> (Left 0, cs)
          (ds, cs1) -> (Left (read ds), cs1)

      parseNumSep ('_' : cs) = (NumSepDash, cs)
      parseNumSep (',' : cs) = (NumSepComma, cs)
      parseNumSep cs         = (NumSepNone, cs)

      parsePrecision ('.' : '{' : cs) =
        case L.break (== '}') cs of
          (ks, '}' : cs1) -> (Right (read ks), cs1)
          _               -> errorCloseTag $ stack cs
      parsePrecision ('.' : cs)       =
        case L.break (not . isDigit) cs of
          ("", cs1) -> (Left 0, cs)
          (ds, cs1) -> (Left (read ds), cs1)
      parsePrecision cs               = (Left (-1), cs)


prettyFmtAlign :: FmtAlign -> String
prettyFmtAlign AlignNone   = ""
prettyFmtAlign AlignLeft   = "<"
prettyFmtAlign AlignRight  = ">"
prettyFmtAlign AlignCenter = "^"
prettyFmtAlign AlignSign   = "="


prettyFmtSign :: FmtSign -> String
prettyFmtSign SignNone  = ""
prettyFmtSign SignPlus  = "+"
prettyFmtSign SignMinus = "-"
prettyFmtSign SignSpace = " "


prettyFmtNumSep :: FmtNumSep -> String
prettyFmtNumSep NumSepNone  = ""
prettyFmtNumSep NumSepDash  = "_"
prettyFmtNumSep NumSepComma = ","


prettyArgFmt :: ArgFmt -> String
prettyArgFmt (ArgFmt{fmtRaw=raw@(_:_)}) = raw
prettyArgFmt (ArgFmt{..}) = concat $
    [pad, align, sign, alternate, aware, width, sep, ".", precision, fmtSpecs]
  where
    showInt i = if i == 0 then "" else show i
    pad = if fmtAlign == AlignNone then "" else [fmtPad]
    align = prettyFmtAlign fmtAlign
    sign = prettyFmtSign fmtSign
    alternate = if fmtAlternate then "#" else ""
    aware = if fmtSignAware then "0" else ""
    width = either showInt (('{' :) . (++ "}") . show) fmtWidth
    sep = prettyFmtNumSep fmtNumSep
    precision = either showInt (('{' :) . (++ "}") . show) fmtPrecision


formatText :: ArgFmt -> ShowS
formatText fmt@ArgFmt{fmtWidth=(Left minw), fmtPrecision=(Left maxw)} cs
    | padw > 0 = pad (fmtAlign fmt) padw (fmtPad fmt)
    | otherwise = cs1
  where
    cs1 = if maxw > 0 && maxw < length cs then take maxw cs else cs
    padw = minw - (length cs1)

    pad :: FmtAlign -> Int -> Char -> String
    pad AlignNone   n c = pad AlignLeft n c
    pad AlignLeft   n c = cs1 ++ replicate n c
    pad AlignRight  n c = replicate n c ++ cs1
    pad AlignCenter n c =
      let ln = div n 2
      in replicate ln c ++ cs1 ++ (replicate (n - ln) c)
formatText fmt _ = errorNoParse $ prettyArgFmt fmt


formatNumber :: ArgFmt -> Bool -> Int -> Maybe Char -> ShowS
formatNumber fmt signed sepWidth flag cs = uncurry (++) $
    pad (fmtAlign fmt) (fmtPad fmt) (fmtSignAware fmt) $
      second (seperate (fmtNumSep fmt) sepWidth) $
        sign (fmtSign fmt) signed $
          alternate (fmtAlternate fmt) flag cs
  where
    pad :: FmtAlign -> Char -> Bool -> (String, String) -> (String, String)
    pad AlignNone   c  False (ps, cs) = (ps, cs)
    pad AlignNone   c  True  (ps, cs) = pad AlignSign '0' False (ps, cs)
    pad AlignLeft   c  _     (ps, cs) = (ps, cs ++ replicate (padw ps cs) c)
    pad AlignRight  c  _     (ps, cs) = (replicate (padw ps cs) c ++ ps, cs)
    pad AlignSign   c  True  (ps, cs) = pad AlignSign c False (ps, cs)
    pad AlignSign  '0' False (ps, cs) =
      let cs1 = fixSep (fmtNumSep fmt) sepWidth (padw ps cs) cs
      in (ps, cs1)
    pad AlignSign   c  False (ps, cs) = (ps, replicate (padw ps cs) c ++ cs)
    pad AlignCenter c  _     (ps, cs) =
      let n = padw ps cs; ln = div n 2
      in (replicate ln c ++ ps, cs ++ (replicate (n - ln) c))

    padw :: String -> String -> Int
    padw ps cs = max 0 $
      case fmtWidth fmt of Left n -> n - (length ps) - (length cs); _ -> 0

    fixSep :: FmtNumSep -> Int -> Int -> String -> String
    fixSep NumSepNone _ n cs = replicate n '0' ++ cs
    fixSep sep        w n cs =
      let sepChar = if sep == NumSepComma then ',' else '_'
          (cs1, cs2) = L.break (== sepChar) cs
          (wc, r) = divMod (n + length cs1) (w + 1)
          n1 = n - wc + if r > 0 then 0 else 1
       in rseperate sepChar w (reverse cs1 ++ replicate n1 '0') ++ cs2

    seperate :: FmtNumSep -> Int -> String -> String
    seperate NumSepNone _ cs = cs
    seperate sep        w cs =
      let sepChar = if sep == NumSepComma then ',' else '_'
          (cs1, cs2) = L.break (== '.') cs
      in rseperate sepChar w $ reverse cs1

    rseperate :: Char -> Int -> String -> String
    rseperate sep w cs =
      case L.splitAt w cs of
        (cs1, "")  -> reverse cs1
        (cs1, cs2) -> rseperate sep w cs2 ++ (sep : reverse cs1)

    sign :: FmtSign -> Bool -> (String, String) -> (String, String)
    sign SignNone  True (ps, cs)       = sign SignMinus True (ps, cs)
    sign SignPlus  True (ps, '-' : cs) = ('-' : ps, cs)
    sign SignPlus  True (ps, '+' : cs) = ('+' : ps, cs)
    sign SignPlus  True (ps, cs)       = ('+' : ps, cs)
    sign SignMinus True (ps, '-': cs)  = ('-' : ps, cs)
    sign SignSpace True (ps, '+' : cs) = (' ' : ps, cs)
    sign SignSpace True (ps, '-' : cs) = ('-' : ps, cs)
    sign SignSpace True (ps, cs)       = (' ' : ps, cs)
    sign _         _    (ps, '+' : cs) = (ps, cs)
    sign _         _    (ps, '-' : cs) = (ps, cs)
    sign _         _    (ps, cs)       = (ps, cs)

    alternate :: Bool -> Maybe Char -> String -> (String, String)
    alternate True (Just c) cs = (['0', c], cs)
    alternate _    _        cs = ("", cs)
