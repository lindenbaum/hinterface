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

import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.ByteString                as BS
import           Data.Word

import           Util.IOx
import           Util.Socket
import           Util.BufferedSocket
import           Language.Erlang.NodeState
import           Language.Erlang.NodeData
import           Language.Erlang.Epmd
import           Language.Erlang.Handshake
import           Language.Erlang.Term
import           Language.Erlang.ControlMessage
import           Language.Erlang.Connection
import           Language.Erlang.Mailbox

--------------------------------------------------------------------------------
data LocalNode = LocalNode { nodeData     :: NodeData
                           , dFlags       :: DistributionFlags
                           , hostName     :: BS.ByteString
                           , registration :: Maybe NodeRegistration
                           , nodeState    :: NodeState Term Term Mailbox Connection
                           , cookie       :: BS.ByteString
                           , acceptor     :: Maybe (Socket, ThreadId)
                           }

--------------------------------------------------------------------------------
newLocalNode :: Term -> BS.ByteString -> IOx LocalNode
newLocalNode nodeName cookie = do
    let (aliveName, hostName) = splitNodeName nodeName
        nodeData = NodeData 0 HiddenNode TcpIpV4 R6B R6B aliveName ""
        localFlags = DistributionFlags [ EXTENDED_REFERENCES
                                       , FUN_TAGS
                                       , NEW_FUN_TAGS
                                       , EXTENDED_PIDS_PORTS
                                       , BIT_BINARIES
                                       , NEW_FLOATS
                                       ]
    LocalNode <$> pure nodeData
              <*> pure localFlags
              <*> pure hostName
              <*> pure Nothing
              <*> newNodeState
              <*> pure cookie
              <*> pure Nothing

registerLocalNode :: LocalNode -> IOx LocalNode
registerLocalNode localNode@LocalNode{nodeData,dFlags,hostName,nodeState,cookie} = do
    (sock, portNo) <- serverSocket hostName
    let nodeData' = nodeData { portNo }
    threadId <- forkIOx (acceptLoop sock nodeData')
    registration <- Just <$> registerNode nodeData' hostName
    return localNode { nodeData = nodeData', registration, acceptor = Just (sock, threadId) }
  where
    acceptLoop sock nodeData' = do
        _ <- acceptConnection sock (getNodeName localNode) nodeData' dFlags nodeState cookie
        acceptLoop sock nodeData'

register :: LocalNode -> Term -> Term -> IOx ()
register LocalNode{nodeState} name pid = do
    mbox <- getMailboxForPid nodeState pid
    putMailboxForName nodeState name mbox

getNodeName :: LocalNode -> BS.ByteString
getNodeName LocalNode{nodeData = NodeData{aliveName},hostName} =
    aliveName `BS.append` "@" `BS.append` hostName

--------------------------------------------------------------------------------
make_pid :: LocalNode -> IOx Term
make_pid localNode@LocalNode{registration,nodeState} = do
    (id, serial) <- new_pid nodeState
    return $ pid (getNodeName localNode) id serial (getCreation registration)

make_ref :: LocalNode -> IOx Term
make_ref localNode@LocalNode{registration,nodeState} = do
    (refId0, refId1, refId2) <- new_ref nodeState
    return $ ref (getNodeName localNode) (getCreation registration) [ refId0, refId1, refId2 ]

make_port :: LocalNode -> IOx Term
make_port localNode@LocalNode{registration,nodeState} = do
    id <- new_port nodeState
    return $ port (getNodeName localNode) id (getCreation registration)

make_mailbox :: LocalNode -> IOx Mailbox
make_mailbox localNode@LocalNode{nodeData,dFlags,nodeState,cookie} = do
    self <- make_pid localNode
    queue <- toIOx newTQueueIO
    let mailbox = newMailbox nodeState self queue make_connection
    putMailboxForPid nodeState self mailbox
    return mailbox
  where
    make_connection :: Term -> IOx Connection
    make_connection remoteName = do
        connectNodes (getNodeName localNode) nodeData dFlags remoteName cookie nodeState

getCreation :: (Maybe NodeRegistration) -> Word8
getCreation = maybe 0 (fromIntegral . nr_creation)

newMailbox :: NodeState Term Term Mailbox Connection -> Term -> TQueue Term -> (Term -> IOx Connection) -> Mailbox
newMailbox nodeState self queue connect = do
    Mailbox self
            _deliverLink
            _deliverSend
            _deliverExit
            _deliverUnlink
            _deliverRegSend
            _deliverGroupLeader
            _deliverExit2
            _sendReg
            _receive
  where
    _deliverLink :: Term -> IOx ()
    _deliverLink _fromPid = do
        undefined

    _deliverSend :: Term -> IOx ()
    _deliverSend message = do
        atomicallyX $ writeTQueue queue message

    _deliverExit :: Term -> Term -> IOx ()
    _deliverExit _fromPid _reason = do
        undefined

    _deliverUnlink :: Term -> IOx ()
    _deliverUnlink _fromPid = do
        undefined

    _deliverRegSend :: Term -> Term -> IOx ()
    _deliverRegSend _fromPid message = do
        atomicallyX $ writeTQueue queue message

    _deliverGroupLeader :: Term -> IOx ()
    _deliverGroupLeader _fromPid = do
        undefined

    _deliverExit2 :: Term -> Term -> IOx ()
    _deliverExit2 _fromPid _reason = do
        undefined

    _sendReg :: Term -> Term -> Term -> IOx ()
    _sendReg regName nodeName message = do
        connection <- getConnectionForNode nodeState nodeName `catchX` const (connect nodeName)
        sendControlMessage connection $ REG_SEND self regName message

    _receive :: IOx Term
    _receive = do
        atomicallyX $ readTQueue queue

--------------------------------------------------------------------------------
closeLocalNode :: LocalNode -> IOx ()
closeLocalNode LocalNode{registration,nodeState,acceptor} = do
    maybe (return ()) (socketClose . nr_sock) registration
    getConnectedNodes nodeState >>= mapM_ (closeConnection . snd)
    maybe (return ()) closeAcceptor acceptor
  where
    closeAcceptor (sock, threadId) = do
        killThreadX threadId
        closeSock sock
