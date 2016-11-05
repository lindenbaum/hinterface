module Foreign.Erlang.Epmd
    ( -- * List registered nodes
      epmdNames
    , NamesResponse(..)
      -- * Looking up nodes
    , lookupNode
      -- * Registering nodes
    , registerNode
    , NodeRegistration(..)
    ) where

import           Control.Monad
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
    put _ = putWithLength16be $ do
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
        return $ NamesResponse (fromIntegral epmdPortNo) (mapMaybe nodeInfo (CL.lines nodeInfos))
      where
        nodeInfo :: CL.ByteString -> Maybe NodeInfo
        nodeInfo cl = do
            [ "name", name, "at", "port", portString ] <- Just $ CL.split ' ' cl
            (port, "") <- CL.readInt portString
            return $ NodeInfo (CL.unpack name) (fromIntegral port)

-- | List all registered nodes
epmdNames :: (MonadBaseControl IO m, MonadLoggerIO m)
          => BS.ByteString -- ^ hostname
          -> m (Either BufferedIOxException NamesResponse)
epmdNames hostName = bracket (connectBufferedSocket hostName)
                             closeBuffered
                             (sendRequest NamesRequest)

data LookupNodeRequest = LookupNodeRequest BS.ByteString
    deriving (Eq, Show)

instance Binary LookupNodeRequest where
    put (LookupNodeRequest alive) =
        putWithLength16be $ do
            putWord8 port_please2_req
            putByteString alive
    get = undefined

data LookupNodeResponse =
      LookupNodeResponse { unLookupNodeResponse :: Maybe NodeData }
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
lookupNode :: (MonadBaseControl IO m, MonadLoggerIO m)
           => BS.ByteString -- ^ alive
           -> BS.ByteString -- ^ hostname
           -> m (Maybe NodeData)
lookupNode alive hostName =
    either (const Nothing) unLookupNodeResponse <$> bracket (connectBufferedSocket hostName)
                                                    closeBuffered
                                                    (sendRequest (LookupNodeRequest alive))

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
                                       then do
                                           return Nothing
                                       else do
                                           creation <- getWord16be
                                           return (Just creation)

data NodeRegistration = NodeRegistration { nr_sock     :: BufferedSocket
                                         , nr_creation :: Word16
                                         }

-- | Register a node
registerNode :: (MonadBaseControl IO m, MonadLoggerIO m)
             => NodeData -- ^ node
             -> BS.ByteString -- ^ hostName
             -> m (Maybe NodeRegistration)
registerNode node hostName = connectCatch >>= mapM sendRegReq >>= return . join
  where
    connectCatch = tryAny (connectBufferedSocket hostName)
        >>= either (logError . fromString . show >=> const (return Nothing)) (return . Just)
    sendRegReq sock = do
        r <- tryAny (sendRequest (RegisterNodeRequest node) sock)
        case r of
            Right (Right (RegisterNodeResponse (Just creation))) ->
                return $ Just $ NodeRegistration sock creation
            x -> do
                logError (fromString (show x))
                closeBuffered sock
                return Nothing

--------------------------------------------------------------------------------
sendRequest :: (MonadLoggerIO m, BufferedIOx s, Binary a, Binary b)
            => a
            -> s
            -> m (Either BufferedIOxException b)
sendRequest req sock = do
    runPutBuffered sock req
    runGetBuffered sock

connectBufferedSocket :: (MonadLoggerIO m)
                      => BS.ByteString -- ^ hostName
                      -> m BufferedSocket
connectBufferedSocket hostName = do
    s <- connectSocket hostName epmdPort
    makeBuffered s
