{-# LANGUAGE Strict              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wno-unused-do-bind #-}

module Foreign.Erlang.Connection
    ( Connection(closeConnection)
    , newConnection
    , sendControlMessage
    ) where

import           Control.Monad
import           Control.Concurrent.STM
import qualified Control.Concurrent.Async      as A
import           Util.BufferedIOx
import           Util.IOExtra
import           Foreign.Erlang.NodeState
import           Foreign.Erlang.Term
import           Foreign.Erlang.ControlMessage
import           Foreign.Erlang.Mailbox

--------------------------------------------------------------------------------
data Connection = MkConnection { sendQueue       :: TQueue ControlMessage
                               , closeConnection :: IO ()
                               }

--------------------------------------------------------------------------------
newConnection :: (MonadLoggerIO m, MonadCatch m, MonadBaseControl IO m, BufferedIOx s, Forall (Pure m))
              => s
              -> NodeState Pid Term Mailbox Connection
              -> Term
              -> m Connection
newConnection sock nodeState name = do
    sendQueue <- liftIO newTQueueIO
    forkTransmitter sendQueue
  where
    forkTransmitter sendQueue = do
        s <- async newSender
        r <- async newReceiver
        let connection = MkConnection sendQueue (stopTransmitter s r)
        liftIO (putConnectionForNode nodeState name connection)
        async (awaitStopAndCleanup s r)
        return connection
      where
        newSender = sendLoop sock sendQueue
        newReceiver = recvLoop sock sendQueue nodeState name
        stopTransmitter s r = void (A.concurrently (A.cancel s) (A.cancel r))
        awaitStopAndCleanup s r = do
            waitCatch r
            cancel s
            tryAndLogAll (liftIO (closeBuffered sock))
            liftIO (removeConnectionForNode nodeState name)

-- liftIO $
-- getConnectionForNode nodeState name >>=
--     mapM_ closeConnection
sendControlMessage :: MonadIO m => ControlMessage -> Connection -> m ()
sendControlMessage controlMessage MkConnection{sendQueue} =
    liftIO $
        atomically $ writeTQueue sendQueue controlMessage

sendLoop :: (MonadCatch m, MonadLoggerIO m, BufferedIOx s)
         => s
         -> TQueue ControlMessage
         -> m ()
sendLoop sock sendQueue =
    forever (tryAndLogIO send)
  where
    send = liftIO $ do
        controlMessage <- atomically $ readTQueue sendQueue
        runPutBuffered sock controlMessage

recvLoop :: (MonadLoggerIO m, MonadCatch m, BufferedIOx s)
         => s
         -> TQueue ControlMessage
         -> NodeState Pid Term Mailbox Connection
         -> Term
         -> m ()
recvLoop sock sendQueue nodeState name =
    forever (recv `catchAll`
                 (\x -> do
                      logErrorShow (x, nodeState)
                      throwM x))
  where
    recv = liftIO (runGetBuffered sock >>= either throwM deliver)
    deliver controlMessage =
        case controlMessage of
            TICK -> atomically $ writeTQueue sendQueue TICK
            LINK fromPid toPid -> do
                mailbox <- getMailboxForPid nodeState toPid
                mapM_ (`deliverLink` fromPid) mailbox
            SEND toPid message -> do
                mailbox <- getMailboxForPid nodeState toPid
                mapM_ (`deliverSend` message) mailbox
            EXIT fromPid toPid reason -> do
                mailbox <- getMailboxForPid nodeState toPid
                mapM_ (\mb -> deliverExit mb fromPid reason) mailbox
            UNLINK fromPid toPid -> do
                mailbox <- getMailboxForPid nodeState toPid
                mapM_ (`deliverUnlink` fromPid) mailbox
            NODE_LINK ->
                -- FIXME
                return ()
            REG_SEND fromPid toName message -> do
                mailbox <- getMailboxForName nodeState toName
                mapM_ (\mb -> deliverRegSend mb fromPid message) mailbox
            GROUP_LEADER fromPid toPid -> do
                mailbox <- getMailboxForPid nodeState toPid
                mapM_ (`deliverGroupLeader` fromPid) mailbox
            EXIT2 fromPid toPid reason -> do
                mailbox <- getMailboxForPid nodeState toPid
                mapM_ (\mb -> deliverExit2 mb fromPid reason) mailbox
