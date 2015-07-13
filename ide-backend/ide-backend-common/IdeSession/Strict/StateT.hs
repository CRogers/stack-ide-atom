{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Version on StateT which evaluates the state strictly at every step
module IdeSession.Strict.StateT (
    -- * Transformer
    StrictStateT(..)
  , modify
  , evalStateT
  , execStateT
    -- * As base monad
  , StrictState
  , runState
  , evalState
  , execState
  ) where

import Control.Applicative
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity

newtype StrictStateT s m a = StrictStateT { runStateT :: s -> m (a, s) }

instance Monad m => Applicative (StrictStateT s m) where
  pure    = return
  f <*> x = do f' <- f ; x' <- x ; return (f' x')

instance Monad m => Monad (StrictStateT s m) where
  return a = StrictStateT $ \s -> return (a, s)
  x >>= f  = StrictStateT $ \s -> do (a, s')  <- runStateT x s
                                     (b, s'') <- runStateT (f a) s'
                                     return (b, s'')

instance Monad m => Functor (StrictStateT s m) where
  f `fmap` m = m >>= return . f

instance Monad m => MonadState s (StrictStateT s m) where
  get     = StrictStateT $ \s -> return (s, s)
  put s   = StrictStateT $ \_ -> s `seq` return ((), s)
  state f = StrictStateT $ \s -> do let (a, s') = f s
                                    s' `seq` return (a, s')

instance MonadTrans (StrictStateT s) where
  lift m = StrictStateT $ \s -> do a <- m
                                   return (a, s)

evalStateT :: Monad m => StrictStateT s m a -> s -> m a
evalStateT m s = do (a, _) <- runStateT m s ; return a

execStateT :: Monad m => StrictStateT s m a -> s -> m s
execStateT m s = do (_, s') <- runStateT m s ; return s'

{------------------------------------------------------------------------------
  As base monad
------------------------------------------------------------------------------}

type StrictState s = StrictStateT s Identity

runState :: StrictState s a -> s -> (a, s)
runState m s = runIdentity $ runStateT m s

evalState :: StrictState s a -> s -> a
evalState m = fst . runState m

execState :: StrictState s a -> s -> s
execState m = snd . runState m
