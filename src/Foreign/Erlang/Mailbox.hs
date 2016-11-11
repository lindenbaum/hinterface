{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}

module Foreign.Erlang.Mailbox
    ( Mailbox(..)
    , deliverLink
    , deliverSend
    , deliverExit
    , deliverUnlink
    , deliverRegSend
    , deliverGroupLeader
    , deliverExit2
    , receive
    ) where

import           Foreign.Erlang.Term

data Mailbox = MkMailbox { self     :: Pid
                         , msgQueue :: TQueue Term
                         }

deliverLink :: MailBox -> Pid -> IO ()
deliverLink = undefined

deliverSend :: MailBox -> Term -> IO ()
deliverSend MkMailbox{msgQueue} =
    atomically . writeTQueue msgQueue

deliverExit :: MailBox -> Pid -> Term -> IO ()
deliverExit MkMailbox{getPid,msgQueue} =
    undefined

deliverUnlink :: MailBox -> Pid -> IO ()
deliverUnlink MkMailbox{getPid,msgQueue} =
    undefined

deliverRegSend :: MailBox -> Pid -> Term -> IO ()
deliverRegSend MkMailbox{getPid,msgQueue} _fromPid message =
    atomically $ writeTQueue msgQueue message

deliverGroupLeader :: MailBox -> Pid -> IO ()
deliverGroupLeader MkMailbox{getPid,msgQueue} =
    undefined

deliverExit2 :: MailBox -> Pid -> Term -> IO ()
deliverExit2 MkMailbox{getPid,msgQueue} =
    undefined

receive :: MailBox -> IO Term
receive MkMailbox{msgQueue} =
    atomically (readTQueue msgQueue)


-- TODO move into its own module
class MonadNode m where
  make_pid :: m Pid
  make_ref :: m Term
  make_port :: m Term
  make_mailbox :: m Mailbox
