{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}

module StackIde where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Stack.Ide.JsonAPI

data StackIde next
  = GetVersion (VersionInfo -> next)
  deriving Functor

makeFree ''StackIde

type StackIdeM = Free StackIde

request :: Request -> IO Response
request req = undefined