{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Foreign.Erlang.LocalNode
    ( LocalNode()
    , runNodeT
    , make_pid
    , make_ref
    , make_port
    , make_mailbox
    , closeLocalNode
    ) where

import           Prelude                       hiding ( id )

import           Control.Applicative           ( (<|>) )
import           Control.Monad
import           Control.Monad.Trans.Maybe     ( MaybeT(..), runMaybeT )
import           Control.Monad.RWS.Strict
import           Control.Concurrent.STM
import qualified Data.ByteString.Char8         as CS
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

-- API:
runNodeT :: (MonadResource m, MonadThrow m, MonadMask m, MonadLogger m, MonadLoggerIO m, MonadIO m)
         => LocalNodeConfig
         -> NodeT m a
         -> m a
data LocalNodeConfig = LocalNodeConfig { aliveName :: String
                                       , hostName  :: String
                                       , cookie    :: String
                                       }
    deriving Show

-- IMPLEMENTATION:
runNodeT LocalNodeConfig{aliveName,hostName,cookie} NodeT{unNodeT} = do
    requireM "(aliveName /= \"\")" (aliveName /= "")
    requireM "(hostName /= \"\")" (hostName /= "")
    bracket setupAcceptorSock stopAllConnections acceptRegisterAndRun
  where
    setupAcceptorSock = do
        let nodeNameBS = CS.pack (aliveName ++ "@" ++ hostName)
        (_, (acceptorSocket, portNo)) <- allocate (serverSocket (CS.pack hostName))
                                                  (closeSock . fst)
        let dFlags = DistributionFlags [ EXTENDED_REFERENCES
                                       , FUN_TAGS
                                       , NEW_FUN_TAGS
                                       , EXTENDED_PIDS_PORTS
                                       , BIT_BINARIES
                                       , NEW_FLOATS
                                       ]
            name = Name { n_distVer = R6B
                        , n_distFlags = dFlags
                        , n_nodeName = nodeNameBS
                        }
            nodeData = NodeData { portNo = portNo
                                , nodeType = HiddenNode
                                , protocol = TcpIpV4
                                , hiVer = R6B
                                , loVer = R6B
                                , aliveName = CS.pack aliveName
                                , extra = ""
                                }
            handshakeData = HandshakeData { name
                                          , nodeData
                                          , cookie = CS.pack cookie
                                          }
        nodeState <- liftIO newNodeState
        return LocalNode { acceptorSocket, handshakeData, nodeState }

    acceptRegisterAndRun localNode@LocalNode{acceptorSocket,handshakeData = hsn@HandshakeData{nodeData},nodeState} =
        withAsync accept (const registerAndRun)
      where
        accept = forever (bracketOnErrorLog (acceptSocket acceptorSocket >>=
                                                 makeBuffered)
                                            closeBuffered
                                            onConnect)
          where
            onConnect sock = do
                remoteName <- doAccept (runPutBuffered sock)
                                       (throwLeftM (runGetBuffered sock))
                                       hsn
                either (logErrorShow . ErrMsg "acceptLoop")
                       (void . newConnection sock nodeState . atom)
                       remoteName

        registerAndRun = registerNode nodeData (CS.pack hostName) go
          where
            go nodeRegistration = do
                let env = RegisteredNode { localNode, nodeRegistration }
                (result, _out) <- evalRWST unNodeT env ()
                return result

    stopAllConnections LocalNode{nodeState} = do
        cs <- liftIO $ getConnectedNodes nodeState
        mapM_ (closeConnection . snd) cs

newtype NodeT m a = NodeT { unNodeT :: RWST RegisteredNode () () m a }
    deriving (Functor, Applicative, Monad, MonadCatch, MonadThrow, MonadMask, MonadLogger, MonadIO, MonadTrans)

deriving instance MonadResource m => MonadResource (NodeT m)

data LocalNode = LocalNode { handshakeData  :: HandshakeData
                           , nodeState      :: NodeState Pid Term Mailbox Connection
                           , acceptorSocket :: Socket
                           }

data RegisteredNode = RegisteredNode { localNode        :: LocalNode
                                     , nodeRegistration :: NodeRegistration
                                     }

splitNodeName :: CS.ByteString -> (CS.ByteString, CS.ByteString)
splitNodeName a = case CS.split (fromIntegral (ord '@')) a of
    [ alive, host ] -> (alive, host)
    _ -> error $ "Illegal node name: " ++ show a

register :: LocalNode -> Term -> Pid -> IO ()
register LocalNode{nodeState} name pid' = do
    mbox <- getMailboxForPid nodeState pid'
    mapM_ (putMailboxForName nodeState name) mbox

make_pid :: LocalNode -> NodeRegistration -> IO Pid
make_pid LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},nodeState} registration = do
    (id, serial) <- new_pid nodeState
    return $ pid n_nodeName id serial (getCreation registration)

make_ref :: LocalNode -> NodeRegistration -> IO Term
make_ref LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},nodeState} registration = do
    (refId0, refId1, refId2) <- new_ref nodeState
    return $
        ref n_nodeName (getCreation registration) [ refId0, refId1, refId2 ]

make_port :: LocalNode -> NodeRegistration -> IO Term
make_port LocalNode{handshakeData = HandshakeData{name = Name{n_nodeName}},nodeState} registration = do
    id <- new_port nodeState
    return $ port n_nodeName id (getCreation registration)

make_mailbox :: MonadIO m => LocalNode -> NodeRegistration -> m Mailbox
make_mailbox localNode@LocalNode{handshakeData = handshakeData@HandshakeData{nodeData},nodeState} registration = do
    self <- make_pid localNode registration
    queue <- newTQueueIO
    let mailbox = newMailbox self queue
    putMailboxForPid nodeState self mailbox
    return mailbox
  where
    newMailbox self queue = Mailbox { getPid = self
                                    , deliverLink = \_fromPid -> undefined
                                    , deliverSend = \message ->
                                        atomically $ writeTQueue queue message
                                    , deliverExit = \_fromPid _reason ->
                                        undefined
                                    , deliverUnlink = \_fromPid -> undefined
                                    , deliverRegSend = \_fromPid message ->
                                        atomically $ writeTQueue queue message
                                    , deliverGroupLeader = \_fromPid ->
                                        undefined
                                    , deliverExit2 = \_fromPid _reason ->
                                        undefined
                                    , send = \toPid message ->
                                        void $
                                            runMaybeT $ do
                                                let nodeName = node (toTerm toPid)
                                                c <- connect (atom_name nodeName)
                                                             nodeName
                                                lift (sendControlMessage (SEND toPid
                                                                               message)
                                                                         c)
                                    , sendReg = \regName nodeName message ->
                                        void $
                                            runMaybeT $ do
                                                c <- connect (atom_name nodeName)
                                                             nodeName
                                                lift (sendControlMessage (REG_SEND self
                                                                                   regName
                                                                                   message)
                                                                         c)
                                    , receive = atomically (readTQueue queue)
                                    }
      where
        connect remoteName nodeName =
            MaybeT (getConnectionForNode nodeState nodeName) <|>
                (do
                     n <- MaybeT (lookupNode remoteAlive remoteHost) <|>
                              warnAndReturn
                     lift $ work n)
          where
            (remoteAlive, remoteHost) =
                splitNodeName remoteName
            warnAndReturn = MaybeT $ do
                logWarnStr
                    $ printf "Connection failed: Node '%s' not found on '%s'."
                             (CS.unpack remoteAlive)
                             (CS.unpack remoteHost)
                return Nothing
            work NodeData{portNo = remotePort} =
                bracketOnErrorLog (liftIO (connectSocket remoteHost remotePort) >>=
                                       makeBuffered)
                                  closeBuffered
                                  go
              where
                go sock = do
                    doConnect (runPutBuffered sock)
                              (throwLeftM (runGetBuffered sock))
                              handshakeData
                    newConnection sock nodeState (atom remoteName)

getCreation :: NodeRegistration -> Word8
getCreation = fromIntegral . nr_creation
