{-# LANGUAGE Strict #-}
module Language.Erlang.Connection
    ( Connection()
    , newConnection
    , sendControlMessage
    , closeConnection
    ) where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Util.BufferedIOx
import           Util.IOExtra
import           Language.Erlang.NodeState
import           Language.Erlang.Term
import           Language.Erlang.ControlMessage
import           Language.Erlang.Mailbox

--------------------------------------------------------------------------------
data Connection = Connection { sendQueue :: TQueue ControlMessage
                             , onClose   :: IO ()
                             }

--------------------------------------------------------------------------------
newConnection :: (BufferedIOx s) => s -> NodeState Pid Term Mailbox Connection -> Term -> IO Connection
newConnection sock nodeState name = do
    (sendQueue, sendThread) <- (newSender sendLoop) sock
    recvThread <- (newReceiver recvLoop (sendQueue, nodeState, name)) sock
    let connection = Connection sendQueue (onClose sendThread recvThread)
    putConnectionForNode nodeState name connection
    return connection
  where
    newSender :: (s -> (TQueue m) -> IO ()) -> s -> IO (TQueue m, ThreadId)
    newSender f s = do
        q <- newTQueueIO
        t <- forkIO (f s q)
        return (q, t)
    newReceiver :: (s -> (TQueue m, r, n) -> IO ()) -> (TQueue m, r, n) -> s -> IO ThreadId
    newReceiver f q s = do
        t <- forkIO (f s q)
        return t
    onClose s r = do
        removeConnectionForNode nodeState name
        killThread s
        killThread r
        closeBuffered sock

sendControlMessage :: Connection -> ControlMessage -> IO ()
sendControlMessage Connection{sendQueue} controlMessage = do
    atomically $ writeTQueue sendQueue controlMessage

closeConnection :: Connection -> IO ()
closeConnection Connection{onClose} = do
    onClose

--------------------------------------------------------------------------------
sendLoop :: (BufferedIOx s) => s -> (TQueue ControlMessage) -> IO ()
sendLoop sock sendQueue =
    forever (send `catch` logX "send")
  where
    send = do
        controlMessage <- atomically $ readTQueue sendQueue
        runPutBuffered sock controlMessage

recvLoop :: (BufferedIOx s) => s -> (TQueue ControlMessage, NodeState Pid Term Mailbox Connection, Term) -> IO ()
recvLoop sock (sendQueue, nodeState, name) = do
    forever (recv `catch`
                 (\x -> do
                      logX "recv" x
                      getConnectionForNode nodeState name >>= closeConnection
                      throw x))
  where
    recv = do
        controlMessage <- runGetBuffered sock
        deliver controlMessage `catch` logX "deliver"
    deliver controlMessage = do
        case controlMessage of
            TICK -> do
                atomically $ writeTQueue sendQueue TICK
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
