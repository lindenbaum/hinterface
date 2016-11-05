{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}

module Foreign.Erlang.Mailbox ( Mailbox(..) ) where

import           Util.IOExtra

import           Foreign.Erlang.Term

data Mailbox = Mailbox { getPid             :: Pid
                       , deliverLink        :: forall m. (MonadLoggerIO m) => Pid -> m ()
                       , deliverSend        :: forall m. (MonadLoggerIO m) => Term -> m ()
                       , deliverExit        :: forall m. (MonadLoggerIO m) => Pid -> Term -> m ()
                       , deliverUnlink      :: forall m. (MonadLoggerIO m) => Pid -> m ()
                       , deliverRegSend     :: forall m. (MonadLoggerIO m) => Pid -> Term -> m ()
                       , deliverGroupLeader :: forall m. (MonadLoggerIO m) => Pid -> m ()
                       , deliverExit2       :: forall m. (MonadLoggerIO m) => Pid -> Term -> m ()
                       , send               :: forall m. (MonadLoggerIO m) => Pid -> Term -> m ()
                       , sendReg            :: forall m. (MonadLoggerIO m) => Term -> Term -> Term -> m ()
                       , receive            :: forall m. (MonadLoggerIO m) => m Term
                       }
