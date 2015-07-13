{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Exception
    (
    module Control.Exception,
    module Exception
    )
    where

import Prelude hiding (catch)

import Control.Exception

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = flip catchIO

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | A monad that can catch exceptions.  A minimal definition
-- requires a definition of 'gcatch'.
--
-- Implementations on top of 'IO' should implement 'gblock' and 'gunblock' to
-- eventually call the primitives 'Control.Exception.block' and
-- 'Control.Exception.unblock' respectively.  These are used for
-- implementations that support asynchronous exceptions.  The default
-- implementations of 'gbracket' and 'gfinally' use 'gblock' and 'gunblock'
-- thus rarely require overriding.
--
class Monad m => ExceptionMonad m where

  -- | Generalised version of 'Control.Exception.catch', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gcatch :: Exception e => m a -> (e -> m a) -> m a

  -- | Generalised version of 'Control.Exception.mask_', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gmask :: ((m a -> m a) -> m b) -> m b

  -- | Generalised version of 'Control.Exception.bracket', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gbracket :: m a -> (a -> m b) -> (a -> m c) -> m c

  -- | Generalised version of 'Control.Exception.finally', allowing an arbitrary
  -- exception handling monad instead of just 'IO'.
  gfinally :: m a -> m b -> m a

  -- | DEPRECATED, here for backwards compatibilty.  Instances can
  -- define either 'gmask', or both 'block' and 'unblock'.
  gblock   :: m a -> m a
  -- | DEPRECATED, here for backwards compatibilty  Instances can
  -- define either 'gmask', or both 'block' and 'unblock'.
  gunblock :: m a -> m a
  -- XXX we're keeping these two methods for the time being because we
  -- have to interact with Haskeline's MonadException class which
  -- still has block/unblock; see GhciMonad.hs.

  gmask    f = gblock (f gunblock)
  gblock   f = gmask (\_ -> f)
  gunblock f = f -- XXX wrong; better override this if you need it

  gbracket before after thing =
    gmask $ \restore -> do
      a <- before
      r <- restore (thing a) `gonException` after a
      _ <- after a
      return r

  a `gfinally` sequel =
    gmask $ \restore -> do
      r <- restore a `gonException` sequel
      _ <- sequel
      return r

#if __GLASGOW_HASKELL__ < 613
instance ExceptionMonad IO where
  gcatch    = catch
  gmask f   = block $ f unblock
  gblock    = block
  gunblock  = unblock
#else
instance ExceptionMonad IO where
  gcatch    = catch
  gmask f   = mask (\x -> f x)
  gblock    = undefined
  gunblock  = undefined
#endif

gtry :: (ExceptionMonad m, Exception e) => m a -> m (Either e a)
gtry act = gcatch (act >>= \a -> return (Right a))
                  (\e -> return (Left e))

-- | Generalised version of 'Control.Exception.handle', allowing an arbitrary
-- exception handling monad instead of just 'IO'.
ghandle :: (ExceptionMonad m, Exception e) => (e -> m a) -> m a -> m a
ghandle = flip gcatch

-- | Always executes the first argument.  If this throws an exception the
-- second argument is executed and the exception is raised again.
gonException :: (ExceptionMonad m) => m a -> m b -> m a
gonException ioA cleanup = ioA `gcatch` \e ->
                             do _ <- cleanup
                                throw (e :: SomeException)

main :: IO ()
main = return ()