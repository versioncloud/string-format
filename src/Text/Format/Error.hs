module Text.Format.Error
  ( ArgError(..)
  , isArgKeyError
  , catchArgError
  , vferror
  , errorNoParse
  , errorCloseTag
  , errorArgKey
  , errorArgFmt
  ) where


import           Control.Exception


data ArgError = ArgKeyError | ArgFmtError deriving (Show, Eq)

instance Exception ArgError

isArgKeyError :: SomeException -> Bool
isArgKeyError = maybe False (== ArgKeyError) . fromException

catchArgError :: a -> a -> SomeException -> a
catchArgError key fmt e = maybe (throw e) handle (fromException e)
  where
    handle ArgKeyError = key
    handle ArgFmtError = fmt


--------------------------------------------------------------------------------
vferror :: String -> a
vferror = errorWithoutStackTrace . ("vformat: " ++)


--------------------------------------------------------------------------------
-- *** Exception: vformat: no parse "xxx"
errorNoParse :: String -> a
errorNoParse = vferror . ("no parse " ++) . show

-- *** Exception: vformat: "xxx" close tag '}' missing
errorCloseTag :: String -> a
errorCloseTag = vferror . (++ " close tag '}' missing") . show

-- *** Exception: vformat: bad arg key "xxx"
errorArgKey :: String -> a
errorArgKey = vferror . ("bad arg key " ++) . show

-- *** Exception: vformat: bad arg format "xxx"
errorArgFmt :: String -> a
errorArgFmt = vferror . ("bad arg format " ++) . show
