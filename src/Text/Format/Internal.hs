{-# LANGUAGE TypeOperators #-}

module Text.Format.Internal
  ( (:=) (..)
  , ferror
  , errorArgFmt
  , errorCloseTag
  , errorTypeFmt
  , errorMissingArg
  ) where


-- | A type represent a named ArgKey and an another data
infixr 6 :=
data (:=) a = String := a


--------------------------------------------------------------------------------
ferror :: String -> a
ferror s = errorWithoutStackTrace $ "format: " ++ s

errorArgFmt :: String -> a
errorArgFmt cs = ferror $ "bad arg format: " ++ cs

errorCloseTag :: a
errorCloseTag = ferror $ "close tag '}' missing"

errorTypeFmt :: String -> String -> a
errorTypeFmt ts cs = ferror $ ts ++ " not allowed for " ++ cs ++ " type(s)"

errorMissingArg :: a
errorMissingArg = ferror "argument missing"
