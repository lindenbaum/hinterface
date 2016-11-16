{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module Foreign.Erlang.LocalNode
    ( LocalNode()
    , runNodeT
    , make_pid
    , make_ref
    , make_port
    , make_mailbox
    , closeLocalNode
    ) where

import           Prelude                              hiding ( id )

import           Control.Applicative                  ( (<|>) )
import           Control.Monad
import           Control.Monad.Trans.Maybe            ( MaybeT(..), runMaybeT )
import           Control.Monad.RWS.Strict
import           Control.Concurrent.STM
import           Control.Monad.Base
import qualified Data.ByteString.Char8                as CS
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

runNodeT :: forall m a.
         (MonadResource m, MonadThrow m, MonadMask m, MonadLogger m, MonadLoggerIO m, MonadIO m, Forall (Pure m))
         => LocalNodeConfig
         -> NodeT m a
         -> m a

data LocalNodeConfig = LocalNodeConfig { aliveName :: String
                                       , hostName  :: String
                                       , cookie    :: String
                                       }
    deriving Show

-- IMPLEMENTATION:

class MonadNodeClass m where
    askLocalNode :: m LocalNode
    askNodeRegistration :: m NodeRegistration

newtype NodeT m a = NodeT { unNodeT :: RWST RegisteredNode () () m a }
    deriving (Functor, Applicative, Monad, MonadCatch, MonadThrow, MonadMask, MonadLogger, MonadIO)

deriving instance (MonadBase IO (NodeT m), MonadResource m) =>
         MonadResource (NodeT m)

deriving instance (MonadBase b (NodeT m)) =>
         MonadResource (NodeT m)

data LocalNode = LocalNode { handshakeData  :: HandshakeData
                           , nodeState      :: NodeState Pid Term Mailbox Connection
                           , acceptorSocket :: Socket
                           }

data RegisteredNode = RegisteredNode { localNode        :: LocalNode
                                     , nodeRegistration :: NodeRegistration
                                     }

instance Monad m =>
         MonadNodeClass (NodeT m) where
    askLocalNode = NodeT (asks localNode)
    askNodeRegistration = NodeT (asks nodeRegistration)

askCreation :: (Monad m) => NodeT m Word8
askCreation = fromIntegral . nr_creation <$> askNodeRegistration

askNodeState :: (Monad m) => NodeT m (NodeState Pid Term Mailbox Connection)
askNodeState = nodeState <$> askLocalNode

askNodeName :: (Monad m) => NodeT m CS.ByteString
askNodeName = n_nodeName . name . handshakeData <$> askLocalNode

make_pid :: (MonadIO m, Monad m) => NodeT m Pid
make_pid = do
    name <- askNodeName
    state <- askNodeState
    (id, serial) <- liftIO (new_pid state)
    cr <- askCreation
    return (pid name id serial cr)

register_pid :: (MonadIO m, Monad m) => Term -> Pid -> NodeT m Bool
register_pid name pid' = do
    state <- askNodeState
    liftIO (do
                mbox <- getMailboxForPid state pid'
                mapM_ (putMailboxForName state name) mbox
                return (isJust mbox))

make_ref :: (MonadIO m, Monad m) => NodeT m Term
make_ref = do
    state <- askNodeState
    name <- askNodeName
    (refId0, refId1, refId2) <- liftIO (new_ref state)
    cr <- askCreation
    return (ref name cr [ refId0, refId1, refId2 ])

make_port :: (MonadIO m, Monad m) => NodeT m Term
make_port = do
    name <- askNodeName
    state <- askNodeState
    id <- liftIO (new_port state)
    cr <- askCreation
    return $ port name id cr

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

    acceptRegisterAndRun :: LocalNode -> m a
    acceptRegisterAndRun localNode@LocalNode{acceptorSocket,handshakeData = hsn@HandshakeData{nodeData},nodeState} =
        withAsync accept (\accepted -> link accepted >> registerAndRun)
      where
        accept = forever (bracketOnErrorLog (liftIO (acceptSocket acceptorSocket >>=
                                                         makeBuffered))
                                            (liftIO . closeBuffered)
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

splitNodeName :: CS.ByteString -> (CS.ByteString, CS.ByteString)
splitNodeName a = case CS.split '@' a of
    [ alive, host ] -> (alive, host)
    _ -> error $ "Illegal node name: " ++ show a

getOrCreateConnection :: (MonadMask m, MonadBaseControl IO m, MonadResource m, MonadLoggerIO m, MonadIO m)
                      => CS.ByteString
                      -> NodeT m (Maybe Connection)
getOrCreateConnection remoteName =
  getExistingConnection >>= maybe lookupAndConnect (return . Just)
  where
    getExistingConnection = do
      nodeName <- atom <$> askNodeName
      nodeState <- askNodeState
      getConnectionForNode nodeState nodeName

    lookupAndConnect = lookupNode remoteAlive remoteHost >>= maybe warnNotFound connect
          where
            (remoteAlive, remoteHost) = splitNodeName remoteName
            warnNotFound = do
                logWarnStr
                    (printf "Connection failed: Node '%s' not found on '%s'."
                             (CS.unpack remoteAlive)
                             (CS.unpack remoteHost))
                return Nothing
            connect NodeData{portNo = remotePort} =
               bracketOnErrorLog (liftIO (connectSocket remoteHost
                                           remotePort >>=
                                           makeBuffered))
               cleanup
               go
              where
                cleanup sock = do
                    liftIO (closeBuffered sock)
                    return Nothing
                go sock = Just <$> do
                                   nodeState <- askNodeState
                                   LocalNode{handshakeData} <- askLocalNode
                                   doConnect (runPutBuffered sock)
                                             (throwLeftM (runGetBuffered sock))
                                             handshakeData
                                   newConnection sock
                                                 nodeState
                                                 (atom remoteName)


make_mailbox :: (MonadResource m, MonadLoggerIO m, MonadIO m)
             => NodeT m Mailbox
make_mailbox =
    do
        self <- make_pid
        msgQueue <- liftIO newTQueueIO
        let mailbox = MkMailbox {self, msgQueue}
        nodeState <- askNodeState
        liftIO (putMailboxForPid nodeState self mailbox)
        return mailbox

-- send :: (MonadResource m, MonadLoggerIO m, MonadIO m)
--              => MailBox -> Pid -> Term -> NodeT m ()
-- send MkMailbox{self,msgQueue} toPid message =
--     void $
--         runMaybeT $ do
--             let nodeName = node (toTerm toPid)
--             c <- connect (atom_name nodeName) nodeName
--             lift (sendControlMessage (SEND toPid message) c)

-- sendReg :: (MonadResource m, MonadLoggerIO m, MonadIO m)
--   => MailBox -> Term -> Term -> Term -> NodeT m ()
-- sendReg MkMailbox{self,msgQueue} =
--  void $
--   runMaybeT $ do
--   c <- connect (atom_name nodeName)
--        nodeName
--        lift (sendControlMessage (REG_SEND self
--                                   regName
--                                   message)
--               c)
