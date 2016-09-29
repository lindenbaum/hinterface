module Language.Erlang.Connection
    ( Connection()
    , newConnection
    , sendControlMessage
    , closeConnection
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM

import           Util.BufferedSocket            ( BufferedSocket, socketClose )
import           Util.Util

import           Util.IOx
import           Language.Erlang.NodeState
import           Language.Erlang.Term
import           Language.Erlang.ControlMessage
import           Language.Erlang.Mailbox

--------------------------------------------------------------------------------
data Connection = Connection { sendQueue :: TQueue ControlMessage
                             , onClose   :: IOx ()
                             }

--------------------------------------------------------------------------------
newConnection :: BufferedSocket -> NodeState Term Term Mailbox Connection -> Term -> IOx Connection
newConnection sock nodeState name = do
    (sendQueue, sendThread) <- (newSender sendLoop) sock
    recvThread <- (newReceiver recvLoop (sendQueue, nodeState, name)) sock
    let connection = Connection sendQueue (onClose sendThread recvThread)
    putConnectionForNode nodeState name connection
    return connection
  where
    newSender :: (s -> (TQueue m) -> IOx ()) -> s -> IOx (TQueue m, ThreadId)
    newSender f s = do
        q <- toIOx $ newTQueueIO
        t <- forkIOx (f s q)
        return (q, t)
    newReceiver :: (s -> (TQueue m, r, n) -> IOx ()) -> (TQueue m, r, n) -> s -> IOx ThreadId
    newReceiver f q s = do
        t <- forkIOx (f s q)
        return t
    onClose s r = do
        removeConnectionForNode nodeState name
        killThreadX s
        killThreadX r
        socketClose sock

sendControlMessage :: Connection -> ControlMessage -> IOx ()
sendControlMessage Connection{sendQueue} controlMessage = do
    atomicallyX $ writeTQueue sendQueue controlMessage

closeConnection :: Connection -> IOx ()
closeConnection Connection{onClose} = do
    onClose

--------------------------------------------------------------------------------
sendLoop :: BufferedSocket -> (TQueue ControlMessage) -> IOx ()
sendLoop sock sendQueue = do
    body `catchX` (logX "sendLoop")
    sendLoop sock sendQueue
  where
    body = do
        controlMessage <- atomicallyX $ readTQueue sendQueue
        runPutSocket2 sock controlMessage

recvLoop :: BufferedSocket -> (TQueue ControlMessage, NodeState Term Term Mailbox Connection, Term) -> IOx ()
recvLoop sock (sendQueue, nodeState, name) = do
    body >> recvLoop sock (sendQueue, nodeState, name) `catchX` (logX "recvLoop") >> getConnectionForNode nodeState name >>=
        closeConnection
  where
    body = do
        controlMessage <- runGetSocket2 sock
        case controlMessage of
            TICK -> do
                atomicallyX $ writeTQueue sendQueue TICK
            LINK fromPid toPid -> do
                mailbox <- getMailboxForPid nodeState toPid
                deliverLink mailbox fromPid
            SEND toPid message -> do
                mailbox <- getMailboxForPid nodeState toPid
                deliverSend mailbox message
            EXIT fromPid toPid reason -> do
                mailbox <- getMailboxForPid nodeState toPid
                deliverExit mailbox fromPid reason
            UNLINK fromPid toPid -> do
                mailbox <- getMailboxForPid nodeState toPid
                deliverUnlink mailbox fromPid
            NODE_LINK -> do
                -- FIXME
                return ()
            REG_SEND fromPid toName message -> do
                mailbox <- getMailboxForName nodeState toName
                deliverRegSend mailbox fromPid message
            GROUP_LEADER fromPid toPid -> do
                mailbox <- getMailboxForPid nodeState toPid
                deliverGroupLeader mailbox fromPid
            EXIT2 fromPid toPid reason -> do
                mailbox <- getMailboxForPid nodeState toPid
                deliverExit2 mailbox fromPid reason
