module Text.Format.ArgKey ( ArgKey (..), emptyKey ) where

import           Control.Arrow
import           Data.Char     (isDigit)
import qualified Data.List     as L


{-| A data type indicates key of format argument

==== The key syntax

  @
    key :: [(int | chars) {"!" (int | chars)}]
  @

  Note: See 'Format' to learn more about syntax description language

  Examples

    >>> read "0" :: ArgKey
    >>> read "country" :: ArgKey
    >>> read "coun!!try" :: ArgKey
    >>> read "country!name" :: ArgKey
    >>> read "country!cities!10!name" :: ArgKey
-}
data ArgKey = Index Int           -- ^ Refers to a positional argument or
                                  -- index in a list like argument.
            | Name String         -- ^ Refers to a named argument or a record
                                  -- name of an argument.
            | Nest ArgKey ArgKey  -- ^ Refers to a attribute (index or name) of
                                  -- an argument.
            deriving (Eq, Ord)

instance Read ArgKey where
  readsPrec _ "" = [ (emptyKey, "") ]
  readsPrec _ cs = [ parse cs ]
    where
      parse :: String -> (ArgKey, String)
      parse cs =
        case break cs of
          ("", cs1)  -> (undefined, cs1)
          (_, "!")   -> (undefined, "!")
          (cs1, "")  -> (parse1 cs1, "")
          (cs1, cs2) -> first (Nest $ parse1 cs1) (parse cs2)

      parse1 :: String -> ArgKey
      parse1 cs = if all isDigit cs then Index (read cs) else Name cs

      break :: String -> (String, String)
      break cs =
        case L.break (== '!') cs of
          (cs1, "")              -> (cs1, "")
          (cs1, "!")             -> (cs1, "!")
          (cs1, '!' : '!' : cs2) -> first ((cs1 ++ "!") ++) (break cs2)
          (cs1, '!' : cs2)       -> (cs1, cs2)

instance Show ArgKey where
  show k@(Index i) = if emptyKey == k then "" else show i
  show (Name s)    = escape s
    where
      escape :: String -> String
      escape ""         = ""
      escape ('!' : cs) = "!!" ++ escape cs
      escape (c : cs)   = (c : escape cs)
  show (Nest k1 k2)  = show k1 ++ "!" ++ show k2

emptyKey :: ArgKey
emptyKey = Index (-1)
