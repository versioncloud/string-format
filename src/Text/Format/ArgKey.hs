module Text.Format.ArgKey ( ArgKey (..), emptyKey ) where

import           Control.Arrow
import           Data.Char     (isDigit)
import qualified Data.List     as L


-- | ArgKey indicates a key of format argument
--
--  There are two kinds of basic key, named and indexed,
--  and a composed key indicates a key which is a attribute of
--  an argument.
--
--  When read from a String, the sytax is as followings:
--
--  1. if all chars are digits, it means an indexed key,
--
--  2. if there is a __"!"__, it means a nested key,
--     the chars before __"!"__ is parent key,
--     and the chars after are child key,
--
--  3. if you want to use literal __"!"__ in the key, you can write it doublely,
--     __"!!"__,
--
--  4. if there are not all digits, it's a named key.
--
--  Examples:
--
--  >>> read "country" :: ArgKey
--  Name "country"
--
--  >>> read "123" :: ArgKey
--  Index 123
--
--  >>> read "country!name" :: ArgKey
--  Nest (Name "country") (Name "name")
--
--  >>> read "country!cities!10" :: ArgKey
--  Nest (Name "country") (Nest (Name "cities") (Index 10))
--
--  >>> read "coun!!try" :: ArgKey
--  Name "coun!try"
--
data ArgKey = Index Int
            | Name String
            | Nest ArgKey ArgKey
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
