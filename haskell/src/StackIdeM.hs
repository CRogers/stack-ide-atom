{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}

module StackIdeM (
  VersionInfo(..),
  SourceError(..)
, module StackIdeM
) where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Text (Text)

import IdeSession.Types.Public (SourceError)
import Stack.Ide.JsonAPI

type Directory = Text

data StackIde next
  = CreateSession Directory next
  | GetVersion (VersionInfo -> next)
  | GetSourceErrors ([SourceError] -> next)
  deriving Functor

makeFree ''StackIde

type StackIdeM = Free StackIde
