{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}
module Foreign.Erlang.Connection
    ( Connection(closeConnection)
    , newConnection
    , sendControlMessage
    ) where

import           Control.Monad
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Util.BufferedIOx
import           Util.IOExtra
import           Foreign.Erlang.NodeState
import           Foreign.Erlang.Term
import           Foreign.Erlang.ControlMessage
import           Foreign.Erlang.Mailbox

--------------------------------------------------------------------------------
data Connection = Connection { sendQueue :: TQueue ControlMessage
                             , closeConnection   :: forall m. MonadLoggerIO m => m ()
                             }

--------------------------------------------------------------------------------
newConnection :: (MonadBaseControl IO m, MonadLoggerIO m, BufferedIOx s)
              => s
              -> NodeState Pid Term Mailbox Connection
              -> Term
              -> m Connection
newConnection sock nodeState name = do
    (sendQueue, sendThread) <- newSender
    recvThread <- newReceiver sendQueue
    let connection = Connection sendQueue (onClose sendThread recvThread)
    putConnectionForNode nodeState name connection
    return connection
  where
    newSender = do
        q <- liftIO newTQueueIO
        t <- fork (sendLoop sock q)
        return (q, t)
    newReceiver q = fork (recvLoop sock q nodeState name)
    onClose s r = do
        removeConnectionForNode nodeState name
        liftIO $ do
            killThread s
            killThread r
        closeBuffered sock

sendControlMessage :: (MonadLoggerIO m) => Connection -> ControlMessage -> m ()
sendControlMessage Connection{sendQueue} controlMessage =
    liftIO $
        atomically $ writeTQueue sendQueue controlMessage

--------------------------------------------------------------------------------
sendLoop :: (MonadBaseControl IO m, MonadLoggerIO m, BufferedIOx s)
         => s
         -> TQueue ControlMessage
         -> m ()
sendLoop sock sendQueue =
    forever (send `catchAny` (logError . fromString . show))
  where
    send = do
        controlMessage <- liftIO $ atomically $ readTQueue sendQueue
        runPutBuffered sock controlMessage

recvLoop :: (MonadLoggerIO m, MonadBaseControl IO m, BufferedIOx s)
         => s
         -> TQueue ControlMessage
         -> NodeState Pid Term Mailbox Connection
         -> Term
         -> m ()
recvLoop sock sendQueue nodeState name = forever recv
  where
    recv = do
        controlMessage <- tryAny $ runGetBuffered sock
        either die (either die deliver) controlMessage
    die x = do
        logError (fromString (show x))
        mc <- getConnectionForNode nodeState name
        closeConnection `mapM_` mc
        throwIO x
    deliver controlMessage =
        go `catchAny` (logError . fromString . show)
      where
        go = case controlMessage of
            TICK -> liftIO $ atomically $ writeTQueue sendQueue TICK
            LINK fromPid toPid -> do
                withMailboxForPid nodeState toPid (flip deliverLink fromPid)
            SEND toPid message -> do
                withMailboxForPid nodeState toPid (flip deliverSend message)
            EXIT fromPid toPid reason -> do
                withMailboxForPid nodeState toPid (\mb -> deliverExit mb fromPid reason)
            UNLINK fromPid toPid -> do
                withMailboxForPid nodeState toPid (flip deliverUnlink fromPid)
            NODE_LINK ->
                -- TODO now tk 12.1.2018
                return ()
            REG_SEND fromPid toName message -> do
                withMailboxForName nodeState toName (\mb -> deliverRegSend mb fromPid message)
            GROUP_LEADER fromPid toPid -> do
                withMailboxForPid nodeState toPid (flip deliverGroupLeader fromPid)
            EXIT2 fromPid toPid reason -> do
                withMailboxForPid nodeState toPid (\mb -> deliverExit2 mb fromPid reason)
