{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}

module Foreign.Erlang.LocalNode
  ( LocalNode (),
    NodeT (),
    LocalNodeConfig (..),
    askCreation,
    askNodeName,
    askNodeState,
    askNodeRegistration,
    askLocalNode,
    runNodeT,
    make_pid,
    make_ref,
    make_port,
    make_mailbox,
    register_pid,
    send,
    sendReg,
  )
where

--    , closeLocalNode

import Control.Monad
-- import Util.IOExtra

import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import Foreign.Erlang.Connection
import Foreign.Erlang.ControlMessage (ControlMessage (..))
import Foreign.Erlang.Epmd
import Foreign.Erlang.Handshake
import Foreign.Erlang.LocalNode.Internal
import Foreign.Erlang.Mailbox
import Foreign.Erlang.NodeData
import Foreign.Erlang.NodeState
import Foreign.Erlang.Term
import Network.BufferedSocket
import Text.Printf (printf)
import UnliftIO
import UnliftIO.Resource
import Util.BufferedIOx
import Util.IOExtra (bracketOnErrorLog, logInfoStr, logWarnStr, requireM, tryAndLogAll)
import Util.Socket
import Prelude hiding (id)

askNodeRegistration :: Monad m => NodeT m NodeRegistration
askNodeRegistration = NodeT (asks nodeRegistration)

askCreation :: Monad m => NodeT m Word32
askCreation = fromIntegral . nr_creation <$> askNodeRegistration

askNodeName :: Monad m => NodeT m Text
askNodeName = n_nodeName . name . handshakeData <$> askLocalNode

make_pid :: MonadIO m => NodeT m Pid
make_pid = do
  name <- askNodeName
  state <- askNodeState
  (id, serial) <- liftIO (new_pid state)
  cr <- askCreation
  return (MkPid (NewPid SmallAtomUtf8 name id serial cr))

register_pid :: (MonadIO m) => Text -> Pid -> NodeT m Bool
register_pid name pid' = do
  state <- askNodeState
  liftIO
    ( do
        mbox <- getMailboxForPid state pid'
        mapM_ (putMailboxForName state name) mbox
        return (isJust mbox)
    )

make_ref :: (MonadIO m) => NodeT m Term
make_ref = do
  state <- askNodeState
  name <- askNodeName
  (refId0, refId1, refId2) <- liftIO (new_ref state)
  cr <- askCreation
  return (NewerReference SmallAtomUtf8 name cr [refId0, refId1, refId2])

make_port :: (MonadIO m) => NodeT m Term
make_port = do
  name <- askNodeName
  state <- askNodeState
  id <- liftIO (new_port state)
  cr <- askCreation
  return $ NewPort SmallAtomUtf8 name id cr

runNodeT ::
  (MonadResource m, MonadUnliftIO m, MonadLogger m, MonadLoggerIO m) =>
  LocalNodeConfig ->
  NodeT m a ->
  m a
runNodeT LocalNodeConfig {aliveName, hostName, cookie} action = do
  requireM "(aliveName /= \"\")" (aliveName /= "")
  requireM "(hostName /= \"\")" (hostName /= "")
  bracket setupAcceptorSock stopAllConnections acceptRegisterAndRun
  where
    setupAcceptorSock = do
      (_, (acceptorSocket, portNo)) <-
        allocate
          (serverSocket hostName)
          (closeSock . fst)
      let name = otp23Name (aliveName <> "@" <> hostName)
          nodeData =
            NodeData
              { portNo = portNo,
                nodeType = HiddenNode,
                protocol = TcpIpV4,
                hiVer = R6B,
                loVer = R6B,
                aliveName = aliveName,
                extra = ""
              }
          handshakeData = HandshakeData {name, nodeData, cookie}
      nodeState <- liftIO newNodeState
      return LocalNode {acceptorSocket, handshakeData, nodeState}

    acceptRegisterAndRun localNode@LocalNode {acceptorSocket, handshakeData = hsn@HandshakeData {nodeData}, nodeState} =
      withAsync accept (\accepted -> link accepted >> registerAndRun)
      where
        accept =
          forever
            ( bracketOnErrorLog
                ( liftIO
                    ( acceptSocket acceptorSocket
                        >>= makeBuffered
                    )
                )
                (liftIO . closeBuffered)
                onConnect
            )
          where
            onConnect sock =
              tryAndLogAll
                ( doAccept
                    (runPutBuffered sock)
                    (runGetBuffered sock)
                    hsn
                )
                >>= maybe
                  (return ())
                  (void . newConnection sock nodeState)

        registerAndRun = registerNode nodeData hostName go
          where
            go nodeRegistration =
              let env = RegisteredNode {localNode, nodeRegistration}
               in runRegisteredNode env action

    stopAllConnections LocalNode {nodeState} = do
      cs <- liftIO $ getConnectedNodes nodeState
      mapM_ (liftIO . closeConnection . snd) cs

make_mailbox :: (MonadIO m) => NodeT m Mailbox
make_mailbox = do
  self <- make_pid
  msgQueue <- newTQueueIO
  let mailbox = MkMailbox {self, msgQueue}
  nodeState <- askNodeState
  liftIO (putMailboxForPid nodeState self mailbox)
  return mailbox

send ::
  (MonadUnliftIO m, MonadResource m, MonadLoggerIO m) =>
  Pid ->
  Term ->
  NodeT m ()
send toPid message =
  maybe (return Nothing) getOrCreateConnection (nodeNameText (toTerm toPid))
    >>= mapM_ (sendControlMessage (SEND toPid message))

sendReg ::
  (MonadUnliftIO m, MonadResource m, MonadLoggerIO m) =>
  Mailbox ->
  Text ->
  Text ->
  Term ->
  NodeT m ()
sendReg MkMailbox {self} regName nodeNameT message =
  getOrCreateConnection nodeNameT
    >>= mapM_ (sendControlMessage (REG_SEND self regName message))

splitNodeName :: Text -> (Text, Text)
splitNodeName a = case Text.splitOn "@" a of
  [alive, host] -> (alive, host)
  _ -> error $ "Illegal node name: " ++ show a

getOrCreateConnection ::
  (MonadUnliftIO m, MonadResource m, MonadLoggerIO m) =>
  Text ->
  NodeT m (Maybe Connection)
getOrCreateConnection remoteName =
  getExistingConnection >>= maybe lookupAndConnect (return . Just)
  where
    getExistingConnection = do
      logInfoStr (printf "getExistingConnection %s" (Text.unpack remoteName))
      nodeState <- askNodeState
      logNodeState nodeState
      getConnectionForNode nodeState remoteName

    lookupAndConnect =
      lookupNode remoteAlive remoteHost
        >>= maybe warnNotFound connect
      where
        (remoteAlive, remoteHost) =
          splitNodeName remoteName
        warnNotFound = do
          logWarnStr
            ( printf
                "Connection failed: Node '%s' not found on '%s'."
                (Text.unpack remoteAlive)
                (Text.unpack remoteHost)
            )
          return Nothing
        connect NodeData {portNo = remotePort} =
          bracketOnErrorLog
            ( liftIO
                ( connectSocket remoteHost remotePort
                    >>= makeBuffered
                )
            )
            cleanup
            go
          where
            cleanup sock = do
              liftIO (closeBuffered sock)
              return Nothing
            go sock =
              Just <$> do
                nodeState <- askNodeState
                LocalNode {handshakeData} <- askLocalNode
                doConnect
                  (runPutBuffered sock)
                  (runGetBuffered sock)
                  handshakeData
                newConnection sock nodeState remoteName
