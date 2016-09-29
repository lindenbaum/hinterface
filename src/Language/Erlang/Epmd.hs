module Language.Erlang.Epmd
    ( -- * List registered nodes
      epmdNames
    , NamesResponse(..)
      -- * Looking up nodes
    , lookupNode
      -- * Registering nodes
    , registerNode
    , NodeRegistration(..)
    ) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get
import           Data.Maybe

import           Data.IOx
import           Util.BufferedSocket
import           Util.Binary
import           Util.Socket
import           Util.Util
import           Language.Erlang.NodeData

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
        return $ NamesResponse (fromIntegral epmdPortNo) (catMaybes (map nodeInfo (CL.lines nodeInfos)))
      where
        nodeInfo :: CL.ByteString -> Maybe NodeInfo
        nodeInfo cl = do
            [ "name", name, "at", "port", portString ] <- Just $ CL.split ' ' cl
            (port, "") <- CL.readInt portString
            return $ NodeInfo (CL.unpack name) (fromIntegral port)

-- | List all registered nodes
epmdNames :: BS.ByteString -- ^ hostname
          -> IOx NamesResponse
epmdNames hostName = do
    withBufferedSocket hostName $ sendRequest NamesRequest

--------------------------------------------------------------------------------
data LookupNodeRequest = LookupNodeRequest BS.ByteString
    deriving (Eq, Show)

instance Binary LookupNodeRequest where
    put (LookupNodeRequest alive) =
        putWithLength16be $ do
            putWord8 port_please2_req
            putByteString alive
    get = undefined

data LookupNodeResponse = LookupNodeResponse (Maybe NodeData)
    deriving (Eq, Show)

instance Binary LookupNodeResponse where
    put _ = undefined
    get = LookupNodeResponse <$> do
                                 matchWord8 port_please2_resp
                                 result <- getWord8
                                 if result > 0
                                     then do
                                         return $ Nothing
                                     else do
                                         Just <$> get

-- | Lookup a node
lookupNode :: BS.ByteString -- ^ alive
           -> BS.ByteString -- ^ hostname
           -> IOx NodeData
lookupNode alive hostName = do
    LookupNodeResponse r <- withBufferedSocket hostName $ sendRequest (LookupNodeRequest alive)
    case r of
        (Just n) -> return n
        Nothing -> errorX doesNotExistErrorType (show alive)

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
registerNode :: NodeData -- ^ node
             -> BS.ByteString -- ^ hostName
             -> IOx NodeRegistration
registerNode node hostName = do
    sock <- connectBufferedSocket hostName
    RegisterNodeResponse r <- sendRequest (RegisterNodeRequest node) sock
    case r of
        (Just creation) -> do
            return $ NodeRegistration sock creation
        Nothing -> do
            socketClose sock
            errorX alreadyExistsErrorType (show $ aliveName node)

--------------------------------------------------------------------------------
sendRequest :: (Binary a, Binary b) => a -> BufferedSocket -> IOx b
sendRequest req sock = do
    runPutSocket2 sock req
    runGetSocket2 sock

connectBufferedSocket :: BS.ByteString -- ^ hostName
                      -> IOx BufferedSocket
connectBufferedSocket hostName = do
    connectSocket hostName epmdPort >>= makeBuffered

withBufferedSocket :: (Binary b)
                   => BS.ByteString -- ^ hostName
                   -> (BufferedSocket -> IOx b)
                   -> IOx b
withBufferedSocket hostName f = do
    sock <- connectBufferedSocket hostName
    res <- f sock
    socketClose sock
    return res
