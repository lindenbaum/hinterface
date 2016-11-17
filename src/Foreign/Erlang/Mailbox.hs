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

import           Util.IOExtra

import           Foreign.Erlang.Term
import           Control.Concurrent.STM

data Mailbox = MkMailbox { self     :: Pid
                         , msgQueue :: TQueue Term
                         }

deliverLink :: Mailbox -> Pid -> IO ()
deliverLink = undefined

deliverSend :: Mailbox -> Term -> IO ()
deliverSend MkMailbox{msgQueue} =
    atomically . writeTQueue msgQueue

deliverExit :: Mailbox -> Pid -> Term -> IO ()
deliverExit =
    undefined

deliverUnlink :: Mailbox -> Pid -> IO ()
deliverUnlink =
    undefined

deliverRegSend :: Mailbox -> Pid -> Term -> IO ()
deliverRegSend MkMailbox{msgQueue} _fromPid message =
    atomically $ writeTQueue msgQueue message

deliverGroupLeader :: Mailbox -> Pid -> IO ()
deliverGroupLeader =
    undefined

deliverExit2 :: Mailbox -> Pid -> Term -> IO ()
deliverExit2 =
    undefined

receive :: Mailbox -> IO Term
receive MkMailbox{msgQueue} =
    atomically (readTQueue msgQueue)
