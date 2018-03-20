{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Trans.Except.Exception (
    bracket
  , bracketOnError
  ) where


import           Control.Exception (SomeException)
import           Control.Monad (Monad (..), liftM)
import           Control.Monad.Catch (MonadMask (..), catchAll, throwM)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)

import           Data.Either (Either (..), either)
import           Data.Function (($), (.), const, id)


bracket ::
     MonadMask m
  => ExceptT e m a
  -- ^ Acquire
  -> (a -> ExceptT e m c)
  -- ^ Release
  -> (a -> ExceptT e m b)
  -- ^ Do some work
  -> ExceptT e m b
bracket acquire release run =
  ExceptT $ bracketF
    (runExceptT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runExceptT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runExceptT (run r'))
{-# INLINE bracket #-}

bracketOnError ::
     MonadMask m
  => ExceptT e m a
  -- ^ Acquire
  -> (a -> ExceptT e m c)
  -- ^ Release
  -> (a -> ExceptT e m b)
  -- ^ Do some work
  -> ExceptT e m b
bracketOnError acquire release run =
  ExceptT $ bracketOnErrorF
    (runExceptT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runExceptT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runExceptT (run r'))
{-# INLINE bracketOnError #-}

data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `liftM` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `liftM` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        z <- f a'
        return $ either id (const b) z
{-# INLINE bracketF #-}

bracketOnErrorF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketOnErrorF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `liftM` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `liftM` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        return b
{-# INLINE bracketOnErrorF #-}
