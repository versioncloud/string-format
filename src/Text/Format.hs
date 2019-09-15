{-|
Module          : Text.Format
Copyright       : (c) 2019 Version Cloud
License         : BSD3
Maintainer      : Jorah Gao <jorah@version.cloud>
Stability       : experimental
Portability     : portable

'Text.Format' vs [Text.Printf](http://hackage.haskell.org/package/base/docs/Text-Printf.html)

* Printf is more ligth-weight
* Printf is more effective in basic formatting, e.g.:

    @
      printf "%s %d %f" "hello" 123 456.789
      format "{:s} {:d} {:f}" "hello" 123 456.789
    @

* Format is more effective in complex formatting, e.g.:

    @
      printf "%30s %30d %30f" "hello" 123 456.789
      format "{:>30s} {:>30d} {:>30f}" "hello" 123 456.789
    @

* Printf can only consume args in order, e.g.:

    @
      printf "%s %d %f" "hello" 123 456.789
      format "{2:s} {1:d} {0:f}" 456.789 123 "hello"
    @

* Printf can only consume position args, e.g.:

    @
      printf "%s %d %f" "hello" 123 456.789
      format "{hello:s} {int:d} {float:f}" ("hello" := "hello") ("int" := 123)
        ("float" := 456.789)
    @

* Format is easier to implement for a new type, e.g.:

    @
      instance FormatArg UTCTime where
        formatArg x _ fmt = formatTime defaultTimeLocale (fmtSpecs fmt) x

      format "{:%Y-%m-%d}" $ read "2019-01-01" :: UTCTime
    @

    @
      data Student = Student { name     :: String
                             , age      :: Int
                             , email    :: String
                             } deriving Generic

      instance FormatArg Student

      format "{0!name:<20s} {0!age:<10d} {0!email:<20s}" $
        Student "Jorah Gao" 27 "jorah@version.cloud"
    @
-}

module Text.Format
  ( format
  , format1
  , module Text.Format.Class
  , module Text.Format.Format
  , module Text.Format.ArgKey
  , module Text.Format.ArgFmt
  , module Text.Format.Internal
  ) where

import           Data.Map             (empty)

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Class
import           Text.Format.Format
import           Text.Format.Internal


-- | Format a variable number of argument with Python-style formatting string
--
-- >>> format "hello {} {}" "world" "!" :: String
-- hello world !
-- >>> format "hello {1} {0}" "!" "world" :: String
-- hello world !
-- >>> format "hello {to} {bang}" ("to" := "world") ("bang" := "!")
-- hello world !
--
format :: FormatType r => Format -> r
format = flip sfmt empty

-- | Format argument with Python-style formatting string
--
-- >>> :set -XDeriveGeneric
-- >>> data Greeting = Greeting {to :: String, bang :: String} deriving Generic
-- >>> instance FormatArg Greeting
-- >>> format1 "hello {to} {bang}" (Greeting "world" "!")
-- hello world !
-- >>> format "hello {0!to} {0!bang}" (Greeting "world" "!")
-- hello world !
--
format1 :: FormatArg a => Format1 -> a -> String
format1 = format . Format . unFormat1
