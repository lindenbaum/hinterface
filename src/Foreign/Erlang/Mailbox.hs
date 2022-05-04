{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Foreign.Erlang.Mailbox
  ( Mailbox (..),
    deliverLink,
    deliverSend,
    deliverExit,
    deliverUnlink,
    deliverRegSend,
    deliverGroupLeader,
    deliverExit2,
    receive,
  )
where

import Foreign.Erlang.Term

import UnliftIO

data Mailbox = MkMailbox
  { self :: Pid,
    msgQueue :: TQueue Term
  }

deliverLink :: MonadUnliftIO m => Mailbox -> Pid -> m ()
deliverLink = error "not yet implemented: deliverLink"

deliverSend :: MonadUnliftIO m => Mailbox -> Term -> m ()
deliverSend MkMailbox {msgQueue} =
  atomically . writeTQueue msgQueue

deliverExit :: MonadUnliftIO m => Mailbox -> Pid -> Term -> m ()
deliverExit = error "not yet implemented: deliverExit"

deliverUnlink :: MonadUnliftIO m => Mailbox -> Pid -> m ()
deliverUnlink = error "not yet implemented: deliverUnlink"

deliverRegSend :: MonadUnliftIO m => Mailbox -> Pid -> Term -> m ()
deliverRegSend MkMailbox {msgQueue} _fromPid message =
  atomically $ writeTQueue msgQueue message

deliverGroupLeader :: MonadUnliftIO m => Mailbox -> Pid -> m ()
deliverGroupLeader = error "not yet implemented: deliverGroupLeader"

deliverExit2 :: MonadUnliftIO m => Mailbox -> Pid -> Term -> m ()
deliverExit2 = error "not yet implemented: deliverExit2"

receive :: MonadUnliftIO m => Mailbox -> m Term
receive MkMailbox {msgQueue} =
  atomically (readTQueue msgQueue)
