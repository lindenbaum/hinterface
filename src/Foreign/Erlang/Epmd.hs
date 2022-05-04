{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Foreign.Erlang.Epmd
  ( -- * List registered nodes
    epmdNames,
    NamesResponse (..),

    -- * Looking up nodes
    lookupNode,

    -- * Registering nodes
    registerNode,
    NodeRegistration (nr_creation),
    mkTestingNodeRegistration,
  )
where

-- import Util.IOExtra

import Control.Monad (when)
import Control.Monad.Logger
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Maybe
import Foreign.Erlang.NodeData
import Network.BufferedSocket
import UnliftIO
import UnliftIO.Resource
import Util.Binary
import Util.BufferedIOx
import Util.IOExtra (logInfoShow)
import Util.Socket
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

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
  put _ =
    putWithLength16be $
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
    NamesResponse
      (fromIntegral epmdPortNo)
      . mapMaybe nodeInfo
      . CL.lines
      <$> getRemainingLazyByteString
    where
      nodeInfo :: CL.ByteString -> Maybe NodeInfo
      nodeInfo cl = do
        ["name", name, "at", "port", portString] <- Just $ CL.split ' ' cl
        (port, "") <- CL.readInt portString
        return $ NodeInfo (CL.unpack name) (fromIntegral port)

-- | List all registered nodes
epmdNames ::
  (MonadUnliftIO m, MonadResource m, MonadLoggerIO m) =>
  -- | hostname
  Text ->
  m NamesResponse
epmdNames hostName = withBufferedSocket hostName (sendRequest NamesRequest)

--------------------------------------------------------------------------------
newtype LookupNodeRequest = LookupNodeRequest Text
  deriving (Eq, Show)

instance Binary LookupNodeRequest where
  put (LookupNodeRequest alive) =
    putWithLength16be $ do
      putWord8 port_please2_req
      putByteString (Text.encodeUtf8 alive)
  get = undefined

newtype LookupNodeResponse = LookupNodeResponse {fromLookupNodeResponse :: Maybe NodeData}
  deriving (Eq, Show)

instance Binary LookupNodeResponse where
  put _ = undefined
  get =
    LookupNodeResponse <$> do
      matchWord8 port_please2_resp
      result <- getWord8
      if result > 0
        then return Nothing
        else Just <$> get

-- | Lookup a node
lookupNode ::
  (MonadUnliftIO m, MonadResource m, MonadLoggerIO m) =>
  -- | alive
  Text ->
  -- | hostname
  Text ->
  m (Maybe NodeData)
lookupNode alive hostName =
  fromLookupNodeResponse
    <$> withBufferedSocket
      hostName
      ( sendRequest
          (LookupNodeRequest alive)
      )

--------------------------------------------------------------------------------
newtype RegisterNodeRequest = RegisterNodeRequest NodeData
  deriving (Eq, Show)

instance Binary RegisterNodeRequest where
  put (RegisterNodeRequest node) =
    putWithLength16be $ do
      putWord8 alive2_req
      put node
  get = undefined

newtype RegisterNodeResponse = RegisterNodeResponse (Maybe Word16)
  deriving (Eq, Show)

instance Binary RegisterNodeResponse where
  put _ = undefined
  get =
    RegisterNodeResponse <$> do
      matchWord8 alive2_resp
      result <- getWord8
      if result > 0
        then return Nothing
        else Just <$> getWord16be

newtype NodeRegistration = NodeRegistration {nr_creation :: Word16}

newtype NodeAlreadyRegistered = NodeAlreadyRegistered NodeData
  deriving (Show)

instance Exception NodeAlreadyRegistered

-- | Register a node with an epmd; as long as the TCP connection is open, the
-- registration is considered valid.
registerNode ::
  (MonadResource m, MonadLoggerIO m, MonadUnliftIO m) =>
  -- | node
  NodeData ->
  -- | hostName
  Text ->
  -- | action to execute while the TCP connection is alive
  (NodeRegistration -> m a) ->
  m a
registerNode node hostName action =
  withBufferedSocket hostName go
  where
    go sock = do
      r@(RegisterNodeResponse mr) <-
        sendRequest
          (RegisterNodeRequest node)
          sock
      logInfoShow r
      when (isNothing mr) (throwIO (NodeAlreadyRegistered node))
      action (NodeRegistration (fromJust mr))

mkTestingNodeRegistration :: Word16 -> NodeRegistration
mkTestingNodeRegistration = NodeRegistration

sendRequest ::
  (MonadLoggerIO m, MonadUnliftIO m, MonadIO m, BufferedIOx s, Binary a, Binary b) =>
  a ->
  s ->
  m b
sendRequest req sock = do
  runPutBuffered sock req
  runGetBuffered sock

withBufferedSocket ::
  (MonadIO m, MonadUnliftIO m) =>
  -- | hostName
  Text ->
  (BufferedSocket -> m b) ->
  m b
withBufferedSocket hostName =
  bracket (liftIO $ connectBufferedSocket hostName) (liftIO . closeBuffered)

connectBufferedSocket ::
  (MonadIO m) =>
  -- | hostName
  Text ->
  m BufferedSocket
connectBufferedSocket hostName =
  liftIO $
    connectSocket hostName epmdPort >>= makeBuffered
