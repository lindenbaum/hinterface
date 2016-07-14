module Language.Erlang.Mailbox ( Mailbox(..)
                               )
       where

import Util.IOx
import Language.Erlang.Term

--------------------------------------------------------------------------------

data Mailbox = Mailbox { getPid             :: Term
                       , deliverLink        :: Term -> IOx ()
                       , deliverSend        :: Term -> IOx ()
                       , deliverExit        :: Term -> Term -> IOx ()
                       , deliverUnlink      :: Term -> IOx ()
                       , deliverRegSend     :: Term -> Term -> IOx ()
                       , deliverGroupLeader :: Term -> IOx ()
                       , deliverExit2       :: Term -> Term -> IOx ()
                       , sendReg            :: Term -> Term -> Term -> IOx ()
                       , receive            :: IOx Term
                       }

--------------------------------------------------------------------------------
