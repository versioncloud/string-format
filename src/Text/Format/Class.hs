{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Format.Class
  ( Formatter
  , FormatArg(..)
  , FormatType(..)
  , (:=) (..)
  ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Data.Char
import           Data.Either
import           Data.Int
import           Data.List           ((!!))
import           Data.Map            hiding (map)
import           Data.Maybe
import           Data.Time.Format
import           Data.Word
import           GHC.Generics
import           Numeric
import           Numeric.Natural

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Error
import           Text.Format.Format

type Formatter = ArgKey -> ArgFmt -> Either SomeException String


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
  formatArg x = gformatArg (from x)

  keyOf :: a -> ArgKey
  keyOf _ = Index (-1)

instance {-# OVERLAPPABLE #-} FormatTime t => FormatArg t where
  formatArg = throwIfNest $ \x k fmt ->
    let specs = fmtSpecs fmt <|> "%Y-%m-%dT%H:%M:%S"
        x'    = formatTime defaultTimeLocale specs x
        fmt'  = fmt{fmtSpecs=""}
    in formatArg x' k fmt'

instance {-# OVERLAPPABLE #-} FormatArg a => FormatArg [a] where
  formatArg x (Nest _ k@(Index i))          = formatArg (x !! i) (Index (-1))
  formatArg x (Nest _ k@(Nest (Index i) _)) = formatArg (x !! i) k
  formatArg _ _                             = const $ throwM ArgKeyError

instance {-# OVERLAPPABLE #-} FormatArg a => FormatArg (Map String a) where
  formatArg x (Nest _ k@(Name n))          = formatArg (x ! n) (Index (-1))
  formatArg x (Nest _ k@(Nest (Name n) _)) = formatArg (x ! n) k
  formatArg _ _                            = const $ throwM ArgKeyError

instance {-# OVERLAPPABLE #-} FormatArg a => FormatArg (Map Int a) where
  formatArg x (Nest _ k@(Index i))          = formatArg (x ! i) (Index (-1))
  formatArg x (Nest _ k@(Nest (Index i) _)) = formatArg (x ! i) k
  formatArg _ _                             = const $ throwM ArgKeyError

instance FormatArg String where
  formatArg = throwIfNest formatString

instance FormatArg Char where
  formatArg = throwIfNest $ formatInteger False . toInteger . ord

instance FormatArg Int where
  formatArg = throwIfNest $ formatInteger True . toInteger

instance FormatArg Int8 where
  formatArg = throwIfNest $ formatInteger True . toInteger

instance FormatArg Int16 where
  formatArg = throwIfNest $ formatInteger True . toInteger

instance FormatArg Int32 where
  formatArg = throwIfNest $ formatInteger True . toInteger

instance FormatArg Int64 where
  formatArg = throwIfNest $ formatInteger True . toInteger

instance FormatArg Word where
  formatArg = throwIfNest $ formatInteger False . toInteger

instance FormatArg Word8 where
  formatArg = throwIfNest $ formatInteger False . toInteger

instance FormatArg Word16 where
  formatArg = throwIfNest $ formatInteger False . toInteger

instance FormatArg Word32 where
  formatArg = throwIfNest $ formatInteger False . toInteger

instance FormatArg Word64 where
  formatArg = throwIfNest $ formatInteger False . toInteger

instance FormatArg Integer where
  formatArg = throwIfNest $ formatInteger True

instance FormatArg Natural where
  formatArg = throwIfNest $ formatInteger False . toInteger

instance FormatArg Float where
  formatArg = throwIfNest formatRealFloat

instance FormatArg Double where
  formatArg = throwIfNest formatRealFloat


--------------------------------------------------------------------------------
class GFormatArg f where
  gformatArg :: f p -> ArgKey -> ArgFmt -> Either SomeException String

-- Data type
instance GFormatArg f =>  GFormatArg (D1 c f) where
  gformatArg (M1 x) = gformatArg x

-- Choice between Sums
instance (GFormatArg f, GFormatArg g) => GFormatArg (f :+: g) where
  gformatArg (L1 x) = gformatArg x
  gformatArg (R1 x) = gformatArg x

-- Constructor
-- e.g. data GreetTo = Hello { name :: String } | Hi { name :: String }
--      data GreetTo = Hello String | Hi String
--      data Greet = Hello | Hi
instance (Constructor c, GFormatArg f) => GFormatArg (C1 c f) where
  gformatArg c@(M1 x) = gformatArg x

-- Constructor without arguments
-- e.g. data Greet = Hello | Hi
instance {-# OVERLAPPING  #-} Constructor c => GFormatArg (C1 c U1) where
  gformatArg _ (Nest _ _) = const $ throwM ArgKeyError
  gformatArg c k          = formatArg (conName c) k

-- Try Products one by one
instance (GFormatArg f, GFormatArg g) => GFormatArg (f :*: g) where
  gformatArg (x :*: y) k fmt =
      gformatArg x k fmt <|> gformatArg y (dec1 k) fmt
    where
      x <|> y = catchIf isArgKeyError x $ const y

      dec1 :: ArgKey -> ArgKey
      dec1 (Index i)                   = Index (i - 1)
      dec1 (Nest p (Index i))          = Nest p (Index (i - 1))
      dec1 (Nest p (Nest (Index i) k)) = Nest p $ Nest (Index (i - 1)) k
      dec1 k                           = k

-- Selector (record and none record)
-- e.g. data GreetTo = Hello String | Hi String
--      data GreetTo = Hello { name :: String } | Hi { name :: String }
instance (Selector c, GFormatArg f) => GFormatArg (S1 c f) where
  gformatArg s@(M1 x) (Nest _ (Index 0))
    | selName s == "" = gformatArg x (Index (-1))
  gformatArg s@(M1 x) (Nest _ k@(Nest (Index 0) _))
    | selName s == "" = gformatArg x k
  gformatArg s@(M1 x) (Nest _ (Name record))
    | selName s == record = gformatArg x (Index (-1))
  gformatArg s@(M1 x) (Nest _ k@(Nest (Name record) _))
    | selName s == record = gformatArg x k
  gformatArg _ _ = const $ throwM ArgKeyError

-- FormatArg instance
instance (FormatArg c) => GFormatArg (K1 i c) where
  gformatArg (K1 c) = formatArg c


--------------------------------------------------------------------------------
-- | A typeclass provides the variable arguments magic for 'format'
--
class FormatType t where
  sfmt :: Format -> Map ArgKey Formatter -> t

instance (FormatArg a, FormatType r) => FormatType (a -> r) where
  sfmt fmt args = \arg -> sfmt fmt $
      insert (fixIndex $ keyOf arg) (formatArg arg) args
    where
      fixIndex (Index (-1)) = Index $ length [n | Index n <- keys args]
      fixIndex k            = k

instance FormatType String where
  sfmt fmt args = formats (unFormat fmt)
    where
      formats :: [FmtItem] -> String
      formats = concat . (map formats1)

      onError :: (ArgKey, ArgFmt) -> SomeException -> String
      onError (key, fmt) = catchArgError (errorArgKey $ show key)
                                         (errorArgFmt $ prettyArgFmt $ fmt)

      formats1 :: FmtItem -> String
      formats1 (Lit cs)       = cs
      formats1 (Arg key ifmt) =
        either (onError (key, ifmt)) id $ (getFormatter key) key (fixArgFmt ifmt)

      fixArgFmt :: ArgFmt -> ArgFmt
      fixArgFmt ifmt@(ArgFmt{fmtWidth=(Right key)}) =
        fixArgFmt $ ifmt {fmtWidth = Left $ formatWidth key}
      fixArgFmt ifmt@(ArgFmt{fmtPrecision=(Right key)}) =
        fixArgFmt $ ifmt {fmtPrecision = Left $ formatPrecision key}
      fixArgFmt ifmt = ifmt

      formatWidth, formatPrecision :: ArgKey -> Int
      formatWidth key =
        let fmt = read "0.0d"
        in read $ either (onError (key, fmt)) id $ (getFormatter key) key fmt
      formatPrecision = formatWidth

      getFormatter :: ArgKey -> Formatter
      getFormatter (Nest key _)  = getFormatter key
      getFormatter key = fromMaybe (\_ _ -> throwM ArgKeyError) $ args !? key


--------------------------------------------------------------------------------
-- | A type represent a named ArgKey and an another data
data (:=) a = String := a
infixr 6 :=

instance FormatArg a => FormatArg ((:=) a) where
  formatArg (_ := x) (Nest _ k) = formatArg x k
  formatArg (_ := x) _          = formatArg x (Index (-1))

  keyOf (ks := _) = Name ks


--------------------------------------------------------------------------------
formatString :: String -> Formatter
formatString x _ fmt@(ArgFmt{fmtSpecs = ""})  = Right $ formatText fmt x
formatString x _ fmt@(ArgFmt{fmtSpecs = "s"}) = Right $ formatText fmt x
formatString _ _ _                            = throwM ArgFmtError

formatInteger :: Bool -> Integer -> Formatter
formatInteger signed x _ fmt@ArgFmt{fmtSpecs=specs} =
    formatNumber fmt signed (sepw specs) (flag specs) <$> (showx specs x)
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

    encodeSign :: [Char] -> [Char]
    encodeSign "+" = "++"
    encodeSign "-" = "--"
    encodeSign cs  = cs

    showx :: String -> Integer -> Either SomeException String
    showx specs x
      | x < 0 = ('-' :) <$> showx specs (-x)
    showx "" x    = showx "d" x
    showx "b" x   = Right $ showIntAtBase 2 intToDigit x ""
    showx "c" x   = Right $ encodeSign $ [chr $ fromInteger x]
    showx "d" x   = Right $ show x
    showx "o" x   = Right $ showIntAtBase 8 intToDigit x ""
    showx "x" x   = Right $ showIntAtBase 16 intToDigit x ""
    showx "X" x   = map toUpper <$> showx "x" x
    showx _ _     = throwM ArgFmtError

formatRealFloat :: RealFloat a => a -> Formatter
formatRealFloat x _ fmt@ArgFmt{fmtSpecs=specs, fmtPrecision=prec} =
    formatNumber fmt True 3 Nothing <$> showx specs prec1 x
  where
    prec1 = either (\i -> Just $ if i < 0 then 6 else i) (const $ Just 0) prec

    showx :: RealFloat a
          => String -> Maybe Int -> a -> Either SomeException String
    showx specs p x
      | x < 0 = ('-' :) <$> showx specs p (-x)
    showx "" p x    = showx "g" p x
    showx "e" p x   = Right $ showEFloat p x ""
    showx "E" p x   = map toUpper <$> showx "e" p x
    showx "f" p x   = Right $ showFFloat p x ""
    showx "F" p x   = map toUpper <$> showx "f" p x
    showx "g" p x   = Right $ showGFloat p x ""
    showx "G" p x   = map toUpper <$> showx "g" p x
    showx "%" p x   = (++ "%") <$> (showx "f" p (x * 100))
    showx _ _ _     = throwM ArgFmtError


throwIfNest :: (a -> Formatter) -> a -> Formatter
throwIfNest _ _ (Nest _ _) _ = throwM ArgKeyError
throwIfNest f x k fmt        = f x k fmt
