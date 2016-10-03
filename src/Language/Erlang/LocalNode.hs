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

data LocalNode = LocalNode { handshakeNode :: HandshakeNode
                           , registration  :: Maybe NodeRegistration
                           , nodeState     :: NodeState Term Term Mailbox Connection
                           , acceptor      :: Maybe (Socket, ThreadId)
                           }

newLocalNode :: Term -> BS.ByteString -> IOx LocalNode
newLocalNode nodeName cookie = do
    let (aliveName, hostName) = splitNodeName nodeName
        nodeData = NodeData 0 HiddenNode TcpIpV4 R6B R6B aliveName ""
        dFlags = DistributionFlags [ EXTENDED_REFERENCES
                                   , FUN_TAGS
                                   , NEW_FUN_TAGS
                                   , EXTENDED_PIDS_PORTS
                                   , BIT_BINARIES
                                   , NEW_FLOATS
                                   ]
        handshakeNode = HandshakeNode { hostName, dFlags, nodeData, cookie }
    nodeState <- newNodeState
    return LocalNode { handshakeNode, registration = Nothing, nodeState, acceptor = Nothing }

registerLocalNode :: LocalNode -> IOx LocalNode
registerLocalNode localNode@LocalNode{handshakeNode = hsn@HandshakeNode{hostName,nodeData},nodeState} = do
    (sock, portNo) <- serverSocket hostName
    let nodeData' = nodeData { portNo }
        handshakeNode' = hsn { nodeData = nodeData' }
    threadId <- forkIOx (acceptLoop sock handshakeNode')
    registration <- Just <$> registerNode nodeData' hostName
    return localNode { handshakeNode = handshakeNode', registration, acceptor = Just (sock, threadId) }
  where
    acceptLoop sock handshakeNode' =
        forever accept
      where
        accept = do
            sock' <- acceptSocket sock >>= makeBuffered
            remoteName <- doAccept (runPutBuffered sock') (runGetBuffered sock') handshakeNode'
            newConnection sock' nodeState (atom remoteName)

register :: LocalNode -> Term -> Term -> IOx ()
register LocalNode{nodeState} name pid' = do
    mbox <- getMailboxForPid nodeState pid'
    putMailboxForName nodeState name mbox

getNodeName' :: LocalNode -> BS.ByteString
getNodeName' LocalNode{handshakeNode} =
    getNodeName handshakeNode

make_pid :: LocalNode -> IOx Term
make_pid localNode@LocalNode{registration,nodeState} = do
    (id, serial) <- new_pid nodeState
    return $ pid (getNodeName' localNode) id serial (getCreation registration)

make_ref :: LocalNode -> IOx Term
make_ref localNode@LocalNode{registration,nodeState} = do
    (refId0, refId1, refId2) <- new_ref nodeState
    return $ ref (getNodeName' localNode) (getCreation registration) [ refId0, refId1, refId2 ]

make_port :: LocalNode -> IOx Term
make_port localNode@LocalNode{registration,nodeState} = do
    id <- new_port nodeState
    return $ port (getNodeName' localNode) id (getCreation registration)

make_mailbox :: LocalNode -> IOx Mailbox
make_mailbox localNode@LocalNode{handshakeNode = handshakeNode@HandshakeNode{dFlags,nodeData},nodeState} = do
    self <- make_pid localNode
    queue <- toIOx newTQueueIO
    let mailbox = newMailbox nodeState self queue make_connection
    putMailboxForPid nodeState self mailbox
    return mailbox
  where
    make_connection :: Term -> IOx Connection
    make_connection remoteName = do
        let (remoteAlive, remoteHost) =
                splitNodeName remoteName
        remoteNode@NodeData{portNo = remotePort} <- lookupNode remoteAlive remoteHost
        localVersion <- maybeErrorX illegalOperationErrorType
                                    "version mismatch"
                                    (matchDistributionVersion nodeData remoteNode)
        let name = (Name localVersion dFlags (getNodeName handshakeNode))

        sock <- connectSocket remoteHost remotePort >>= makeBuffered

        doConnect (runPutBuffered sock) (runGetBuffered sock) handshakeNode name
        newConnection sock nodeState remoteName

getCreation :: (Maybe NodeRegistration) -> Word8
getCreation = maybe 0 (fromIntegral . nr_creation)

newMailbox :: NodeState Term Term Mailbox Connection -> Term -> TQueue Term -> (Term -> IOx Connection) -> Mailbox
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
                let nodeName = node toPid
                connection <- getConnectionForNode nodeState nodeName `catchX` const (connect nodeName)
                sendControlMessage connection $ SEND toPid message
            , sendReg = \regName nodeName message -> do
                connection <- getConnectionForNode nodeState nodeName `catchX` const (connect nodeName)
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
