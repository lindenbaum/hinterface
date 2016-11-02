{-# LANGUAGE Strict     #-}
{-# LANGUAGE RankNTypes #-}

module Foreign.Erlang.Mailbox ( Mailbox(..) ) where

import           Util.IOExtra

import           Foreign.Erlang.Term

data Mailbox = Mailbox { getPid             :: Pid
                       , deliverLink        :: forall m. (MonadIO m) => Pid -> m ()
                       , deliverSend        :: forall m. (MonadIO m) => Term -> m ()
                       , deliverExit        :: forall m. (MonadIO m) => Pid -> Term -> m ()
                       , deliverUnlink      :: forall m. (MonadIO m) => Pid -> m ()
                       , deliverRegSend     :: forall m. (MonadIO m) => Pid -> Term -> m ()
                       , deliverGroupLeader :: forall m. (MonadIO m) => Pid -> m ()
                       , deliverExit2       :: forall m. (MonadIO m) => Pid -> Term -> m ()
                       , send               :: forall m. (MonadIO m) => Pid -> Term -> m ()
                       , sendReg            :: forall m. (MonadIO m) => Term -> Term -> Term -> m ()
                       , receive            :: forall m. (MonadIO m) => m Term
                       }
