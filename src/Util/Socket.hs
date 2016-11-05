module Util.Socket
    ( connectSocket
    , serverSocket
    , acceptSocket
    , closeSock
    , Socket()
    ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Word
import           Network.Socket        hiding (recv, recvFrom, send, sendTo)

import           Util.IOExtra

--------------------------------------------------------------------------------
connectSocket :: (MonadLoggerIO m) => BS.ByteString -> Word16 -> m Socket
connectSocket hostName portNumber = do
    (sock, sa) <- createSocket hostName (Just portNumber)
    liftIO $ do
        setSocketOption sock NoDelay 1
        connect sock sa
        return sock

serverSocket :: (MonadLoggerIO m) => BS.ByteString -> m (Socket, Word16)
serverSocket hostName = do
    (sock, sa) <- createSocket hostName Nothing
    liftIO $ do
        bind sock sa
        listen sock 5
        port <- socketPort sock
        return (sock, fromIntegral port)

acceptSocket :: (MonadLoggerIO m) => Socket -> m Socket
acceptSocket sock = liftIO $ do
    (sock', _sa) <- accept sock
    setSocketOption sock' NoDelay 1
    return sock'

closeSock :: (MonadLoggerIO m) => Socket -> m ()
closeSock = liftIO . close

createSocket :: (MonadLoggerIO m) => BS.ByteString -> Maybe Word16 -> m (Socket, SockAddr)
createSocket hostName portNumber = do
    ai <- addrInfo hostName portNumber
    sock <- liftIO $ socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
    return (sock, addrAddress ai)

addrInfo :: (MonadLoggerIO m) => BS.ByteString -> Maybe Word16 -> m AddrInfo
addrInfo host port = do
    let hints = defaultHints { addrFlags = [ AI_CANONNAME, AI_NUMERICSERV, AI_ADDRCONFIG ]
                             , addrFamily = AF_INET
                             , addrSocketType = Stream
                             }
    (ai : _) <- liftIO $ getAddrInfo (Just hints) (Just (CS.unpack host)) (show <$> port)
    return ai
