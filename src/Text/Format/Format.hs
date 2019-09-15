module Text.Format.Format
  ( FmtItem(..)
  , Format(..)
  , Format1(..)
  ) where


import           Control.Arrow
import           Data.Char            (isDigit)
import qualified Data.List            as L
import           Data.String

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Internal


data FmtItem = Lit String
             | Arg ArgKey ArgFmt
             deriving (Show, Eq)

-- | Format is a list of 'FmtItem'
--
-- A format contains a variet of literal chars and arguments to be replaced,
-- argument sytax is as follows:
--
-- > {[key][:fmt]}
--
-- * __{}__ means it must be wraped in a pair of braces,
-- * __[]__ means an optional field (or field group),
-- * __key__ is argument's key, see 'ArgKey',
-- * __fmt__ (must leading with a colon) is argument's format, see 'ArgFmt'.
--
-- If you need to include a brace character in the literal text,
-- it can be escaped by doubling: {{ and }}.
--
-- if key is ommited, it means an automically positioned argument.
--
-- Examples:
--
-- >>> unFormat "a left brace {{"
-- [Lit "a left brace {"]
--
-- >>> unFormat "hello {}"
-- [Lit "hello ", Arg (Index 0) (ArgFmt ...)]
--
-- >>> unFormat "{} {}"
-- [Arg (Index 0) (ArgFmt ...), Arg (Index 1) (ArgFmt ...)]
--
-- >>> unFormat "{1} {0}"
-- [Arg (Index 1) (ArgFmt ...), Arg (Index 0) (ArgFmt ...)]
--
-- >>> unFormat "{gender} {age}"
-- [Arg (Name "gender") (ArgFmt ...), Arg (Name "age") (ArgFmt ...)]
--
-- >>> unFormat "{0!gender}"
-- [Arg (Nest (Index 0) (Name "gender")) (ArgFmt ..)]
--
-- >>> unFormat "{:<30s}"
-- [Arg (Index 0) (ArgFmt { fmtAlgin = AlignLeft, fmtWidth = Left 30, ...})]
--
-- >>> unFormat "{:<{width}s}"
-- [Arg (Index 0) (ArgFmt {fmtWidth = Right (Name "width"), ...})]
--
newtype Format = Format { unFormat :: [FmtItem] } deriving (Show, Eq)

instance IsString Format where
  fromString = Format . (fixIndex 0) . parse
    where
      parse :: String -> [FmtItem]
      parse "" = []
      parse cs =
        case parseLiteral cs of
          ("", _)   ->
            case parseArg cs of
              (cs1, Just cs2, cs3) ->
                (Arg (read cs1) (read cs2)) : (parse cs3)
              _ -> error "format error"
          (ls, cs1) -> (Lit ls) : (parse cs1)

      parseLiteral :: String -> (String, String)
      parseLiteral ""               = ("", "")
      parseLiteral ('{' : '{' : cs) = first ('{' :) (parseLiteral cs)
      parseLiteral ('}' : '}' : cs) = first ('{' :) (parseLiteral cs)
      parseLiteral ('{' : cs)       = ([], '{' : cs)
      parseLiteral ('}' : cs)       = ([], '}' : cs)
      parseLiteral (c : cs)         = first (c :) (parseLiteral cs)

      parseArg :: String -> (String, Maybe String, String)
      parseArg cs@('{' : '{' : _) = ("", Nothing, cs)
      parseArg ('{' : cs) =
        case parseArgKey cs of
          (cs1, '}' : cs2) -> (cs1, Just "", cs2)
          (cs1, ':' : cs2) ->
            case parseArgFmt 0 cs2 of
              (cs11, '}' : cs12) -> (cs1, Just cs11, cs12)
              _                  -> errorCloseTag
          _ -> errorCloseTag

      parseArgKey :: String -> (String, String)
      parseArgKey ""           = ("", "")
      parseArgKey cs@('}' : _) = ("", cs)
      parseArgKey cs@(':' : _) = ("", cs)
      parseArgKey (c : cs)     = first (c :) (parseArgKey cs)

      parseArgFmt :: Int -> String -> (String, String)
      parseArgFmt _ ""           = ("", "")
      parseArgFmt 0 cs@('}' : _) = ("", cs)
      parseArgFmt n ('{' : cs)   = first ('{' :) (parseArgFmt (n + 1) cs)
      parseArgFmt n ('}' : cs)   = first ('}' :) (parseArgFmt (n - 1) cs)
      parseArgFmt n (c : cs)     = first (c :) (parseArgFmt n cs)

      fixIndex :: Int -> [FmtItem] -> [FmtItem]
      fixIndex _ [] = []
      -- auto-positioned arg
      fixIndex next ((Arg (Index (-1)) fmt) : items) =
        (Arg (Index next) fmt) : fixIndex (next + 1) items
      -- once there is an explict arg key, auto-position args not working
      fixIndex next items@((Arg _ _) : _) = items
      fixIndex next (item : items) = item : fixIndex next items

-- | A variant of 'Format',
-- it transforms all argument's key to __Nest (Index 0) key__
newtype Format1 = Format1 { unFormat1 :: [FmtItem] } deriving (Show, Eq)

instance IsString Format1 where
  fromString = Format1 . map zeroKey . unFormat . fromString
    where
      zeroKey :: FmtItem -> FmtItem
      zeroKey (Arg key fmt) = Arg (Nest (Index 0) key) fmt
      zeroKey item          = item
