{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Erlang.LocalNode
    ( LocalNode()
    , newLocalNode
    , registerLocalNode
    , register
    , make_pid
    , make_ref
    , make_port
    , make_mailbox
    , closeLocalNode
    ) where

import           Prelude                       hiding ( id )

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.Word

import           Util.IOExtra
import           Util.BufferedIOx
import           Util.Socket
import           Network.BufferedSocket

import           Foreign.Erlang.NodeState
import           Foreign.Erlang.NodeData
import           Foreign.Erlang.Epmd
import           Foreign.Erlang.Handshake
import           Foreign.Erlang.Term
import           Foreign.Erlang.ControlMessage
import           Foreign.Erlang.Connection
import           Foreign.Erlang.Mailbox

data LocalNode = LocalNode { handshakeData :: HandshakeData
                           , registration  :: Maybe NodeRegistration
                           , nodeState     :: NodeState Pid Term Mailbox Connection
                           , acceptor      :: Maybe (Socket, ThreadId)
                           }

newLocalNode :: (MonadIO m) => BS.ByteString -> BS.ByteString -> m LocalNode
newLocalNode nodeName cookie = do
    let dFlags = DistributionFlags [ EXTENDED_REFERENCES
                                   , FUN_TAGS
                                   , NEW_FUN_TAGS
                                   , EXTENDED_PIDS_PORTS
                                   , BIT_BINARIES
                                   , NEW_FLOATS
                                   ]
        name = Name { n_distVer = R6B, n_distFlags = dFlags, n_nodeName = nodeName }
        (aliveName, _hostName) =
            splitNodeName nodeName
        nodeData = NodeData { portNo = 0
                            , nodeType = HiddenNode
                            , protocol = TcpIpV4
                            , hiVer = R6B
                            , loVer = R6B
                            , aliveName
                            , extra = ""
                            }
        handshakeData = HandshakeData { name, nodeData, cookie }
    nodeState <- newNodeState
    return LocalNode { handshakeData, registration = Nothing, nodeState, acceptor = Nothing }

splitNodeName :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitNodeName a = case BS.split (fromIntegral (ord '@')) a of
    [ alive, host ] -> (alive, host)
    _ -> error $ "Illegal node name: " ++ show a

registerLocalNode :: (MonadIO m) => LocalNode -> m LocalNode
registerLocalNode localNode@LocalNode{handshakeData = hsn@HandshakeData{name = Name{n_nodeName},nodeData},nodeState} = do
    let (_, hostName) = splitNodeName n_nodeName
    (sock, portNo) <- liftIO $ serverSocket hostName
    let nodeData' = nodeData { portNo }
        handshakeData' = hsn { nodeData = nodeData' }
    threadId <- liftIO $ forkIO (acceptLoop sock handshakeData')
    registration <- Just <$> (liftIO $ registerNode nodeData' hostName)
    return localNode { handshakeData = handshakeData', registration, acceptor = Just (sock, threadId) }
  where
    acceptLoop sock handshakeData' =
        forever accept
      where
        accept = do
            sock' <- acceptSocket sock >>= makeBuffered
            remoteName <- doAccept (runPutBuffered sock') (runGetBuffered sock') handshakeData'
            newConnection sock' nodeState (atom remoteName)

register :: (MonadIO m) => LocalNode -> Term -> Pid -> m ()
register LocalNode{nodeState} name pid' = do
    mbox <- getMailboxForPid nodeState pid'
    putMailboxForName nodeState name mbox

make_pid :: (MonadIO m) => LocalNode -> m Pid
make_pid LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},registration,nodeState} = do
    (id, serial) <- new_pid nodeState
    return $ pid n_nodeName id serial (getCreation registration)

make_ref :: (MonadIO m) => LocalNode -> m Term
make_ref LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},registration,nodeState} = do
    (refId0, refId1, refId2) <- new_ref nodeState
    return $ ref n_nodeName (getCreation registration) [ refId0, refId1, refId2 ]

make_port :: (MonadIO m) => LocalNode -> m Term
make_port LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},registration,nodeState} = do
    id <- new_port nodeState
    return $ port n_nodeName id (getCreation registration)

make_mailbox :: (MonadIO m) => LocalNode -> m Mailbox
make_mailbox localNode@LocalNode{handshakeData = handshakeData@HandshakeData{nodeData},nodeState} = do
    self <- make_pid localNode
    queue <- liftIO $ newTQueueIO
    let mailbox = newMailbox nodeState self queue make_connection
    putMailboxForPid nodeState self mailbox
    return mailbox
  where
    make_connection :: BS.ByteString -> IO Connection
    make_connection remoteName = do
        let (remoteAlive, remoteHost) =
                splitNodeName remoteName
        remoteNode@NodeData{portNo = remotePort} <- lookupNode remoteAlive remoteHost
        localVersion <- maybeErrorX illegalOperationErrorType
                                    "version mismatch"
                                    (matchDistributionVersion nodeData remoteNode)
        -- let name = (Name localVersion dFlags (getNodeName handshakeData))
        sock <- connectSocket remoteHost remotePort >>= makeBuffered

        doConnect (runPutBuffered sock) (runGetBuffered sock) handshakeData
        newConnection sock nodeState (atom remoteName)

getCreation :: (Maybe NodeRegistration) -> Word8
getCreation = maybe 0 (fromIntegral . nr_creation)

newMailbox :: NodeState Pid Term Mailbox Connection -> Pid -> TQueue Term -> (BS.ByteString -> IO Connection) -> Mailbox
newMailbox nodeState self queue connect =
    Mailbox { getPid = self
            , deliverLink = \_fromPid -> do
                undefined
            , deliverSend = \message -> do
                liftIO $ atomically $ writeTQueue queue message
            , deliverExit = \_fromPid _reason -> do
                undefined
            , deliverUnlink = \_fromPid -> do
                undefined
            , deliverRegSend = \_fromPid message -> do
                liftIO $ atomically $ writeTQueue queue message
            , deliverGroupLeader = \_fromPid -> do
                undefined
            , deliverExit2 = \_fromPid _reason -> do
                undefined
            , send = \toPid message -> liftIO $ do
                let nodeName = node $ toTerm toPid
                connection <- getConnectionForNode nodeState nodeName `catch`
                                  (\(_ :: IOException) -> (connect (atom_name nodeName)))
                sendControlMessage connection $ SEND toPid message
            , sendReg = \regName nodeName message -> liftIO $ do
                connection <- getConnectionForNode nodeState nodeName `catch`
                                  (\(_ :: IOException) -> (connect (atom_name nodeName)))
                sendControlMessage connection $ REG_SEND self regName message
            , receive = do
                liftIO $ atomically $ readTQueue queue
            }

closeLocalNode :: (MonadIO m) => LocalNode -> m ()
closeLocalNode LocalNode{registration,nodeState,acceptor} = do
    maybe (return ()) (closeBuffered . nr_sock) registration
    getConnectedNodes nodeState >>= mapM_ (liftIO . closeConnection . snd)
    maybe (return ()) closeAcceptor acceptor
  where
    closeAcceptor (sock, threadId) = liftIO $ do
        killThread threadId
        closeSock sock
