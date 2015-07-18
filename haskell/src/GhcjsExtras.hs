{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module GhcjsExtras where

import GHCJS.Types
import GHCJS.Foreign

instance Show JSString where
  show = fromJSString

foreign import javascript unsafe
  "$1 === $2" js_str_eq :: JSString -> JSString -> JSBool

instance Eq JSString where
  str1 == str2 = fromJSBool (str1 `js_str_eq` str2)
