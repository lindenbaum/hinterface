module Language.Erlang.LocalNode
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

import           Prelude                        hiding ( id )

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.ByteString                as BS
import           Data.Char
import           Data.Word

import           Data.IOx
import           Util.BufferedIOx
import           Util.Socket
import           Network.BufferedSocket

import           Language.Erlang.NodeState
import           Language.Erlang.NodeData
import           Language.Erlang.Epmd
import           Language.Erlang.Handshake
import           Language.Erlang.Term
import           Language.Erlang.ControlMessage
import           Language.Erlang.Connection
import           Language.Erlang.Mailbox

data LocalNode = LocalNode { handshakeData :: HandshakeData
                           , registration  :: Maybe NodeRegistration
                           , nodeState     :: NodeState Pid Term Mailbox Connection
                           , acceptor      :: Maybe (Socket, ThreadId)
                           }

newLocalNode :: BS.ByteString -> BS.ByteString -> IOx LocalNode
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

registerLocalNode :: LocalNode -> IOx LocalNode
registerLocalNode localNode@LocalNode{handshakeData = hsn@HandshakeData{name = Name{n_nodeName},nodeData},nodeState} = do
    let (_, hostName) = splitNodeName n_nodeName
    (sock, portNo) <- serverSocket hostName
    let nodeData' = nodeData { portNo }
        handshakeData' = hsn { nodeData = nodeData' }
    threadId <- forkIOx (acceptLoop sock handshakeData')
    registration <- Just <$> registerNode nodeData' hostName
    return localNode { handshakeData = handshakeData', registration, acceptor = Just (sock, threadId) }
  where
    acceptLoop sock handshakeData' =
        forever accept
      where
        accept = do
            sock' <- acceptSocket sock >>= makeBuffered
            remoteName <- doAccept (runPutBuffered sock') (runGetBuffered sock') handshakeData'
            newConnection sock' nodeState (atom remoteName)

register :: LocalNode -> Term -> Pid -> IOx ()
register LocalNode{nodeState} name pid' = do
    mbox <- getMailboxForPid nodeState pid'
    putMailboxForName nodeState name mbox

make_pid :: LocalNode -> IOx Pid
make_pid LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},registration,nodeState} = do
    (id, serial) <- new_pid nodeState
    return $ pid n_nodeName id serial (getCreation registration)

make_ref :: LocalNode -> IOx Term
make_ref LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},registration,nodeState} = do
    (refId0, refId1, refId2) <- new_ref nodeState
    return $ ref n_nodeName (getCreation registration) [ refId0, refId1, refId2 ]

make_port :: LocalNode -> IOx Term
make_port LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},registration,nodeState} = do
    id <- new_port nodeState
    return $ port n_nodeName id (getCreation registration)

make_mailbox :: LocalNode -> IOx Mailbox
make_mailbox localNode@LocalNode{handshakeData = handshakeData@HandshakeData{nodeData},nodeState} = do
    self <- make_pid localNode
    queue <- toIOx newTQueueIO
    let mailbox = newMailbox nodeState self queue make_connection
    putMailboxForPid nodeState self mailbox
    return mailbox
  where
    make_connection :: BS.ByteString -> IOx Connection
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

newMailbox :: NodeState Pid Term Mailbox Connection
           -> Pid
           -> TQueue Term
           -> (BS.ByteString -> IOx Connection)
           -> Mailbox
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
            , send = \toPid message -> do
                let nodeName = node $ toTerm toPid
                connection <- getConnectionForNode nodeState nodeName `catchX` const (connect (atom_name nodeName))
                sendControlMessage connection $ SEND toPid message
            , sendReg = \regName nodeName message -> do
                connection <- getConnectionForNode nodeState nodeName `catchX` const (connect (atom_name nodeName))
                sendControlMessage connection $ REG_SEND self regName message
            , receive = do
                liftIO $ atomically $ readTQueue queue
            }

closeLocalNode :: LocalNode -> IOx ()
closeLocalNode LocalNode{registration,nodeState,acceptor} = do
    maybe (return ()) (closeBuffered . nr_sock) registration
    getConnectedNodes nodeState >>= mapM_ (closeConnection . snd)
    maybe (return ()) closeAcceptor acceptor
  where
    closeAcceptor (sock, threadId) = do
        killThreadX threadId
        closeSock sock
