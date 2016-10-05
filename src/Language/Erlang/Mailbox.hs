module Language.Erlang.Mailbox ( Mailbox(..) ) where

import           Data.IOx
import           Language.Erlang.Term

data Mailbox = Mailbox { getPid             :: Pid
                       , deliverLink        :: Pid -> IOx ()
                       , deliverSend        :: Term -> IOx ()
                       , deliverExit        :: Pid -> Term -> IOx ()
                       , deliverUnlink      :: Pid -> IOx ()
                       , deliverRegSend     :: Pid -> Term -> IOx ()
                       , deliverGroupLeader :: Pid -> IOx ()
                       , deliverExit2       :: Pid -> Term -> IOx ()
                       , send               :: Pid -> Term -> IOx ()
                       , sendReg            :: Term -> Term -> Term -> IOx ()
                       , receive            :: IOx Term
                       }
