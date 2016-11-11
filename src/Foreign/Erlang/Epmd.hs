module Foreign.Erlang.Epmd
    ( -- * List registered nodes
      epmdNames
    , NamesResponse(..)
      -- * Looking up nodes
    , lookupNode
      -- * Registering nodes
    , registerNode
    , NodeRegistration(nr_creation)
    ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Maybe

import           Foreign.Erlang.NodeData
import           Network.BufferedSocket
import           Util.Binary
import           Util.BufferedIOx
import           Util.IOExtra
import           Util.Socket

--------------------------------------------------------------------------------
epmdPort :: Word16
epmdPort = 4369

--------------------------------------------------------------------------------
names_req, port_please2_req, port_please2_resp, alive2_req, alive2_resp :: Word8
names_req = 110

port_please2_req = 122

port_please2_resp = 119

alive2_req = 120

alive2_resp = 121

--------------------------------------------------------------------------------
data NamesRequest = NamesRequest
    deriving (Eq, Show)

instance Binary NamesRequest where
    put _ = putWithLength16be $
        putWord8 names_req
    get = undefined

data NodeInfo = NodeInfo String Word16
    deriving (Eq, Show)

data NamesResponse = NamesResponse Word16 [NodeInfo]
    deriving (Eq, Show)

instance Binary NamesResponse where
    put _ = undefined
    get = do
        epmdPortNo <- getWord32be
        nodeInfos <- getRemainingLazyByteString
        return $
            NamesResponse (fromIntegral epmdPortNo)
                          (catMaybes (map nodeInfo (CL.lines nodeInfos)))
      where
        nodeInfo :: CL.ByteString -> Maybe NodeInfo
        nodeInfo cl = do
            [ "name", name, "at", "port", portString ] <- Just $ CL.split ' ' cl
            (port, "") <- CL.readInt portString
            return $ NodeInfo (CL.unpack name) (fromIntegral port)

-- | List all registered nodes
epmdNames :: (MonadMask m, MonadResource m)
          => BS.ByteString -- ^ hostname
          -> m NamesResponse
epmdNames hostName = withBufferedSocket hostName $ sendRequest NamesRequest

--------------------------------------------------------------------------------
newtype LookupNodeRequest =
      LookupNodeRequest { _fromLookupNodeRequest :: BS.ByteString }
    deriving (Eq, Show)

instance Binary LookupNodeRequest where
    put (LookupNodeRequest alive) =
        putWithLength16be $ do
            putWord8 port_please2_req
            putByteString alive
    get = undefined

newtype LookupNodeResponse =
      LookupNodeResponse { fromLookupNodeResponse :: Maybe NodeData }
    deriving (Eq, Show)

instance Binary LookupNodeResponse where
    put _ = undefined
    get = LookupNodeResponse <$> do
                                 matchWord8 port_please2_resp
                                 result <- getWord8
                                 if result > 0
                                     then return Nothing
                                     else Just <$> get

-- | Lookup a node
lookupNode :: (MonadMask m, MonadResource m)
           => BS.ByteString -- ^ alive
           -> BS.ByteString -- ^ hostname
           -> m (Maybe NodeData)
lookupNode alive hostName =
    fromLookupNodeResponse <$> withBufferedSocket hostName
                                                  (sendRequest $
                                                       LookupNodeRequest alive)

--------------------------------------------------------------------------------
data RegisterNodeRequest = RegisterNodeRequest NodeData
    deriving (Eq, Show)

instance Binary RegisterNodeRequest where
    put (RegisterNodeRequest node) =
        putWithLength16be $ do
            putWord8 alive2_req
            put node
    get = undefined

data RegisterNodeResponse = RegisterNodeResponse (Maybe Word16)
    deriving (Eq, Show)

instance Binary RegisterNodeResponse where
    put _ = undefined
    get = RegisterNodeResponse <$> do
                                   matchWord8 alive2_resp
                                   result <- getWord8
                                   if result > 0
                                       then return Nothing
                                       else Just <$> getWord16be

newtype NodeRegistration = NodeRegistration { nr_creation :: Word16 }

newtype NodeAlreadyRegistered = NodeAlreadyRegistered NodeData
    deriving (Show)

instance Exception NodeAlreadyRegistered

-- | Register a node with an epmd; as long as the TCP connection is open, the
-- registration is considered valid.
registerNode :: (MonadResource m, MonadLogger m, MonadMask m)
             => NodeData -- ^ node
             -> BS.ByteString -- ^ hostName
             -> (NodeRegistration -> m a) -- ^ action to execute while the TCP connection is alive
             -> m a
registerNode node hostName action =
    withBufferedSocket hostName go
  where
    go sock = do
        r@(RegisterNodeResponse mr) <- sendRequest (RegisterNodeRequest node)
                                                   sock
        logInfoShow r
        when (isNothing mr) (throwM (NodeAlreadyRegistered node))
        action (NodeRegistration (fromJust mr))

sendRequest :: (MonadMask m, MonadIO m, BufferedIOx s, Binary a, Binary b)
            => a
            -> s
            -> m b
sendRequest req sock = do
    liftIO $ runPutBuffered sock req
    either throwM return =<< runGetBuffered sock

withBufferedSocket :: (MonadIO m, MonadMask m)
                   => BS.ByteString -- ^ hostName
                   -> (BufferedSocket -> m b)
                   -> m b
withBufferedSocket hostName =
    bracket (liftIO $ connectBufferedSocket hostName) (liftIO . closeBuffered)

connectBufferedSocket :: (MonadIO m)
                      => BS.ByteString -- ^ hostName
                      -> m BufferedSocket
connectBufferedSocket hostName =
    liftIO $
        connectSocket hostName epmdPort >>= makeBuffered
