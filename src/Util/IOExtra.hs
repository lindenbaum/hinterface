{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}

module Util.IOExtra
    ( requireM
    , tryAndLogIO
    , tryAndLogAll
    , catchAndLogIO
    , catchAndLogAll
    , onExceptionLog
    , bracketOnErrorLog
    , handleAndLogAll
    , catchAndLog
    , handleAndLog
    , logWarnStr
    , logInfoStr
    , logErrorStr
    , logAndThrow
    , logInfoShow
    , logErrorShow
    , throwLeftM
    , throwNothingM
    , ErrMsg(..)
    , OneBillionDollarBug(..)
    , module X
    ) where

import           Control.Monad                        as X ( unless, void, when )
import           Data.Maybe                           as X ( fromJust, isJust )
import           Control.Exception                    as X ( AssertionFailed(..)
                                                           , Exception(..)
                                                           , SomeException(..) )
import           Control.Monad.Catch                  as X
import           Control.Monad.IO.Class               as X
import           Control.Monad.Logger                 as X ( LoggingT
                                                           , MonadLogger
                                                           , MonadLoggerIO
                                                           , logErrorCS
                                                           , logInfoCS
                                                           , logWarnCS
                                                           , runLoggingT )
import           Control.Monad.Logger.CallStack       as X ( logError, logInfo )
import           Control.Monad.Trans.Control          as X
import           Control.Monad.Trans.Resource         as X
import           Control.Concurrent.Lifted            as X
import           Control.Concurrent.Async.Lifted      as X
import           Text.Printf                          as X
import           Data.Text                            ( Text, pack )
import           GHC.Stack

requireM :: (HasCallStack, MonadCatch m, MonadLogger m)
         => String
         -> Bool
         -> m ()
requireM = requireMCS callStack

requireMCS :: (MonadCatch m, MonadLogger m)
           => CallStack
           -> String
           -> Bool
           -> m ()
requireMCS cs title predicate =
    let e = AssertionFailed title
    in
        unless predicate (logShow logErrorCS cs (ErrMsg title e) >> throwM e)

catchAndLogIO :: (HasCallStack, MonadCatch m, MonadLogger m)
              => m a
              -> (IOError -> m a)
              -> m a
catchAndLogIO = catchAndLog

catchAndLogAll :: (HasCallStack, MonadCatch m, MonadLogger m)
               => m a
               -> (SomeException -> m a)
               -> m a
catchAndLogAll = catchAndLog

bracketOnErrorLog :: (HasCallStack, MonadMask m, MonadLogger m)
                  => m a
                  -> (a -> m b)
                  -> (a -> m c)
                  -> m c
bracketOnErrorLog acquire emergencyCleanup use =
    mask $
        \unmasked -> do
            resource <- acquire
            unmasked (use resource) `onExceptionLog` emergencyCleanup resource

onExceptionLog :: (HasCallStack, MonadCatch m, MonadLogger m)
               => m a
               -> m b
               -> m a
onExceptionLog action handler =
    action `catchAndLogAll` handler'
  where
    handler' e = void handler >> throwM e

handleAndLogAll :: (HasCallStack, MonadCatch m, MonadLogger m)
                => (SomeException -> m a)
                -> m a
                -> m a
handleAndLogAll = handleAndLog

logWarnStr :: (HasCallStack, MonadLogger m) => String -> m ()
logWarnStr = logWarnCS callStack . pack

logInfoStr :: (HasCallStack, MonadLogger m) => String -> m ()
logInfoStr = logInfoCS callStack . pack

logErrorStr :: (HasCallStack, MonadLogger m) => String -> m ()
logErrorStr = logErrorCS callStack . pack

catchAndLog :: (HasCallStack, MonadCatch m, MonadLogger m, Exception e)
            => m a
            -> (e -> m a)
            -> m a
catchAndLog action handler =
    handle (\e -> logErrorCS callStack (pack $ displayException e) >> handler e)
           action

handleAndLog :: (HasCallStack, MonadCatch m, MonadLogger m, Exception e)
             => (e -> m a)
             -> m a
             -> m a
handleAndLog = flip catchAndLog

tryAndLogIO :: (HasCallStack, MonadCatch m, MonadLogger m) => m a -> m (Maybe a)
tryAndLogIO = flip catchAndLogIO (const (pure Nothing)) . fmap Just

tryAndLogAll :: forall a m.
             (HasCallStack, MonadCatch m, MonadLogger m)
             => m a
             -> m (Maybe a)
tryAndLogAll = flip catchAndLog
                    (const (return Nothing) :: SomeException -> m (Maybe a)) .
    fmap Just

logAndThrow :: (HasCallStack, MonadMask m, MonadLogger m, Exception e)
            => e
            -> m a
logAndThrow e = logShow logErrorCS callStack e >> throwM e

logShow :: (Show s) => (CallStack -> Text -> m ()) -> CallStack -> s -> m ()
logShow f cs = f cs . pack . show

logInfoShow :: (HasCallStack, Show s, MonadLogger m) => s -> m ()
logInfoShow = logShow logInfoCS callStack

logErrorShow :: (HasCallStack, Show s, MonadLogger m) => s -> m ()
logErrorShow = logShow logErrorCS callStack

throwLeftM :: (HasCallStack, MonadMask m, MonadLogger m, Exception e)
           => m (Either e r)
           -> m r
throwLeftM = (>>= either logAndThrow return)

throwNothingM :: (HasCallStack, MonadLogger m, MonadCatch m)
              => m (Maybe r)
              -> m r
throwNothingM mmr = do
    mr <- mmr
    requireMCS callStack (show OneBillionDollarBug) (isJust mr)
    return (fromJust mr)

data OneBillionDollarBug = OneBillionDollarBug
    deriving Show

instance Exception OneBillionDollarBug

data ErrMsg a = ErrMsg String a

instance Show a =>
         Show (ErrMsg a) where
    show (ErrMsg title a) = title ++ ": " ++ show a

instance Exception a =>
         Exception (ErrMsg a)
