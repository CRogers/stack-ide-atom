{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}

module StackIdeM (
  VersionInfo(..)
, module StackIdeM
) where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Text (Text)

import Stack.Ide.JsonAPI

type Directory = Text

data StackIde next
  = CreateSession Directory next
  | GetVersion (VersionInfo -> next)
  deriving Functor

makeFree ''StackIde

type StackIdeM = Free StackIde
