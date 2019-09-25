{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Format.Class
  ( Formatter
  , FormatArg(..)
  , FormatType(..)
  ) where

import           Control.Applicative
import           Data.Char
import           Data.Int
import           Data.List            ((!!))
import           Data.Map             hiding (map)
import           Data.Maybe
import           Data.Time.Format
import           Data.Word
import           GHC.Generics
import           Numeric
import           Numeric.Natural

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Format
import           Text.Format.Internal

type Formatter = ArgKey -> ArgFmt -> String


-- | Typeclass of formatable values.
--
-- Make an instance for your own data types:
--
-- @
--  data Coffe = Black | Latte | Other deriving Show
--
--  instance FormatArg Coffe where
--    formatArg x k fmt = formatArg (show x) k fmt
-- @
--
-- @
--  newtype Big a = Big { unBig :: a}
--
--  instance FormatArg a => FormatArg (Big a) where
--    formatArg (Big x) k fmt = formatArg x k fmt
-- @
--
-- @
--  data Student = Student { name     :: String
--                         , age      :: Int
--                         , email    :: String
--                         } deriving Generic
--
--  instance FormatArg Student
-- @
--
-- @
--  data Address = Address { country :: String
--                         , city    :: String
--                         , street  :: String
--                         }
--
--  instance FormatArg Address where
--    formatArg x k fmt = formatArg result k fmt
--      where
--        result :: String
--        result = format "{:s},{:s},{:s}" (street x) (city x) (country x)
-- @
--
class FormatArg a where
  formatArg :: a -> Formatter

  default formatArg :: (Generic a, GFormatArg (Rep a)) => a -> Formatter
  formatArg x = fromMaybe errorMissingArg . gformatArg (from x)

  keyOf :: a -> ArgKey
  keyOf _ = Index (-1)

instance {-# OVERLAPPABLE #-} FormatTime t => FormatArg t where
  formatArg x _ fmt = formatTime defaultTimeLocale specs x
    where
      specs = case fmtSpecs fmt of "" -> "%Y-%m-%dT%H:%M:%S"; cs -> cs

instance {-# OVERLAPPABLE #-} FormatArg a => FormatArg [a] where
  formatArg x (Nest _ k@(Index i)) = formatArg (x !! i) k

instance {-# OVERLAPPABLE #-} FormatArg a => FormatArg (Map String a) where
  formatArg x (Nest _ k@(Name n)) = formatArg (x ! n) k

instance {-# OVERLAPPABLE #-} FormatArg a => FormatArg (Map Int a) where
  formatArg x (Nest _ k@(Index i)) = formatArg (x ! i) k

instance FormatArg a => FormatArg ((:=) a) where
  formatArg (_ := x) = formatArg x
  keyOf (ks := _) = Name ks

instance FormatArg String where
  formatArg = formatString

instance FormatArg Char where
  formatArg = formatInteger False . toInteger . ord

instance FormatArg Int where
  formatArg = formatInteger True . toInteger

instance FormatArg Int8 where
  formatArg = formatInteger True . toInteger

instance FormatArg Int16 where
  formatArg = formatInteger True . toInteger

instance FormatArg Int32 where
  formatArg = formatInteger True . toInteger

instance FormatArg Int64 where
  formatArg = formatInteger True . toInteger

instance FormatArg Word where
  formatArg = formatInteger False . toInteger

instance FormatArg Word8 where
  formatArg = formatInteger False . toInteger

instance FormatArg Word16 where
  formatArg = formatInteger False . toInteger

instance FormatArg Word32 where
  formatArg = formatInteger False . toInteger

instance FormatArg Word64 where
  formatArg = formatInteger False . toInteger

instance FormatArg Integer where
  formatArg = formatInteger True

instance FormatArg Natural where
  formatArg = formatInteger False . toInteger

instance FormatArg Float where
  formatArg = formatRealFloat

instance FormatArg Double where
  formatArg = formatRealFloat


--------------------------------------------------------------------------------
class GFormatArg f where
  gformatArg :: f p -> ArgKey -> Maybe (ArgFmt -> String)

instance GFormatArg V1 where
  gformatArg _ _ = Nothing

instance GFormatArg U1 where
  gformatArg _ _ = Nothing

instance (FormatArg c) => GFormatArg (K1 i c) where
  gformatArg (K1 c) = Just . formatArg c

instance (GFormatArg f, GFormatArg g) => GFormatArg (f :+: g) where
  gformatArg (L1 x) = gformatArg x
  gformatArg (R1 x) = gformatArg x

instance (GFormatArg f, GFormatArg g) => GFormatArg (f :*: g) where
  gformatArg (x :*: y) = (<|>) <$> gformatArg x <*> gformatArg y

instance (GFormatArg f) => GFormatArg (D1 c f) where
  gformatArg (M1 x) = gformatArg x

instance (GFormatArg f) => GFormatArg (C1 c f) where
  gformatArg (M1 x) = gformatArg x

instance (GFormatArg f, Selector c) => GFormatArg (S1 c f) where
  gformatArg s@(M1 x) (Nest _ k@(Name field))
    | selName s == field = gformatArg x k
    | otherwise = Nothing
  gformatArg s@(M1 x) (Nest _ k@(Nest (Name field) _))
    | selName s == field = gformatArg x k
  gformatArg _ _ = Nothing


-- | A typeclass provides the variable arguments magic for 'format'
--
class FormatType t where
  sfmt :: Format -> Map ArgKey Formatter -> t

instance (FormatArg a, FormatType r) => FormatType (a -> r) where
  sfmt fmt args = \arg -> sfmt fmt $
      insert (fixIndex $ keyOf arg) (formatArg arg) args
    where
      nextIndex = 1 + (maximum $ (-1) : [n | Index n <- keys args])
      fixIndex (Index (-1)) = Index nextIndex
      fixIndex k            = k

instance FormatType String where
  sfmt fmt args = formats (unFormat fmt)
    where
      formats :: [FmtItem] -> String
      formats = concat . (map formats1)

      formats1 :: FmtItem -> String
      formats1 (Lit cs)       = cs
      formats1 (Arg key ifmt) = (getFormatter key) key (fixArgFmt ifmt)

      fixArgFmt :: ArgFmt -> ArgFmt
      fixArgFmt ifmt@(ArgFmt _ _ _ _ _ (Right key) _ _ _) =
        fixArgFmt $ ifmt {fmtWidth = Left $ formatWidth key}
      fixArgFmt ifmt@(ArgFmt _ _ _ _ _ _ _ (Right key) _) =
        fixArgFmt $ ifmt {fmtPrecision = Left $ formatPrecision key}
      fixArgFmt ifmt = ifmt

      formatWidth, formatPrecision :: ArgKey -> Int
      formatWidth key = read $ (getFormatter key) key $
        ArgFmt AlignNone ' ' SignNone False False (Left 0) NumSepNone
              (Left 0) "d"
      formatPrecision = formatWidth

      getFormatter :: ArgKey -> Formatter
      getFormatter (Nest key _)  = getFormatter key
      getFormatter key@(Index _) = fromMaybe errorMissingArg $ args !? key
      getFormatter key@(Name _)  = fromMaybe errorMissingArg $ args !? key


--------------------------------------------------------------------------------
formatString :: String -> Formatter
formatString x _ fmt@(ArgFmt{fmtSpecs = ""})  = formatText fmt x
formatString x _ fmt@(ArgFmt{fmtSpecs = "s"}) = formatText fmt x
formatString _ _ _                            = errorArgFmt "unknown specs"

formatInteger :: Bool -> Integer -> Formatter
formatInteger signed x _ fmt@ArgFmt{fmtSpecs=specs} =
    formatNumber fmt signed (sepw specs) (flag specs) (showx specs x)
  where
    sepw :: String -> Int
    sepw "b" = 4
    sepw "o" = 4
    sepw "x" = 4
    sepw "X" = 4
    sepw _   = 3

    flag :: String -> Maybe Char
    flag "b" = Just 'b'
    flag "o" = Just 'o'
    flag "x" = Just 'x'
    flag "X" = Just 'X'
    flag _   = Nothing

    showx :: String -> Integer -> String
    showx specs x | x < 0 = '-' : showx specs (-x)
    showx "" x    = showx "d" x
    showx "b" x   = showIntAtBase 2 intToDigit x ""
    showx "c" x   = [chr $ fromInteger x]
    showx "d" x   = show x
    showx "o" x   = showIntAtBase 8 intToDigit x ""
    showx "x" x   = showIntAtBase 16 intToDigit x ""
    showx "X" x   = map toUpper $ showx "x" x
    showx _ _     = errorArgFmt "unknown spec"

formatRealFloat :: RealFloat a => a -> Formatter
formatRealFloat x _ fmt@ArgFmt{fmtSpecs=specs, fmtPrecision=prec} =
    formatNumber fmt True 3 Nothing $ showx specs prec1 x
  where
    prec1 = either (\n -> Just $ if n < 0 then 6 else n) (const $ Just 6) prec

    showx :: RealFloat a => String -> Maybe Int -> a -> String
    showx specs p x | x < 0 = '-' : showx specs p (-x)
    showx "" p x    = showx "g" p x
    showx "e" p x   = showEFloat p x ""
    showx "E" p x   = map toUpper $ showx "e" p x
    showx "f" p x   = showFFloat p x ""
    showx "F" p x   = map toUpper $ showx "f" p x
    showx "g" p x   = showGFloat p x ""
    showx "G" p x   = map toUpper $ showx "g" p x
    showx "%" p x   = (showx "f" p (x * 100)) ++ "%"
    showx _ _ _     = errorArgFmt "unknown specs"
