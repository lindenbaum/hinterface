module Language.Erlang.Epmd (
  -- * List registered nodes
  epmdNames,
  NamesResponse(..),
  -- * Looking up nodes
  lookupNode,
  -- * Registering nodes
  registerNode,
  RegisterNodeResponse(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Util.IOx
import Util.BufferedSocket
import Util.Binary
import Util.Socket
import Util.Util
import Language.Erlang.NodeData

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

data NamesResponse = NamesResponse Word16 [String]
                   deriving (Eq, Show)

putEpmdNamesRequest :: Put
putEpmdNamesRequest = do
  putWord8 names_req

getEpmdNamesResponse :: Get NamesResponse
getEpmdNamesResponse = do
  epmdPortNo <- getWord32be
  nodeInfos <- getRemainingLazyByteString
  return $ NamesResponse (fromIntegral epmdPortNo) (map CL.unpack (filter (not . CL.null) (CL.lines nodeInfos)))

-- | List all registered nodes
epmdNames :: BS.ByteString -- ^ hostname
          -> IOx NamesResponse
epmdNames hostName = do
  sock <- connectSocket hostName epmdPort >>= makeBuffered
  runPutSocket sock (putWithLength16be putEpmdNamesRequest)
  namesResponse <- runGetSocket sock getEpmdNamesResponse
  socketClose sock
  return namesResponse

--------------------------------------------------------------------------------

putLookupNodeRequest :: BS.ByteString -> Put
putLookupNodeRequest alive = do
  putWord8 port_please2_req
  putByteString alive

getLookupNodeResponse :: Get (Maybe NodeData)
getLookupNodeResponse = do
  matchWord8 port_please2_resp
  result <- getWord8
  if result > 0 then do
    return Nothing
  else do
    Just <$> get

lookupNode :: BS.ByteString -> BS.ByteString -> IOx NodeData
lookupNode alive hostName = do
  sock <- connectSocket hostName epmdPort >>= makeBuffered
  runPutSocket sock (putWithLength16be (putLookupNodeRequest alive))
  r <- runGetSocket sock getLookupNodeResponse
  socketClose sock
  case r of
   (Just n) -> return n
   Nothing  -> errorX doesNotExistErrorType (show alive)

--------------------------------------------------------------------------------

data RegisterNodeResponse = RegisterNodeResponse BufferedSocket Word16

putRegisterNodeRequest :: NodeData -> Put
putRegisterNodeRequest node = do
  putWord8 alive2_req
  put node

getRegisterNodeResponse :: Get (Maybe Word16)
getRegisterNodeResponse = do
  matchWord8 alive2_resp
  result <- getWord8
  if result > 0 then do
    return Nothing
  else do
    creation <- getWord16be
    return (Just creation)

registerNode :: NodeData -> BS.ByteString -> IOx RegisterNodeResponse
registerNode node hostName = do
  sock <- connectSocket hostName epmdPort >>= makeBuffered
  runPutSocket sock (putWithLength16be (putRegisterNodeRequest node))
  r <- runGetSocket sock getRegisterNodeResponse
  case r of
    (Just creation) -> do
      return $ RegisterNodeResponse sock creation
    Nothing -> do
      socketClose sock
      errorX alreadyExistsErrorType (show $ aliveName node)

--------------------------------------------------------------------------------
