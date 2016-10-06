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

--------------------------------------------------------------------------------
connectSocket :: BS.ByteString -> Word16 -> IO Socket
connectSocket hostName portNumber = do
    (sock, sa) <- createSocket hostName (Just portNumber)
    setSocketOption sock NoDelay 1
    connect sock sa
    return sock

serverSocket :: BS.ByteString -> IO (Socket, Word16)
serverSocket hostName = do
    (sock, sa) <- createSocket hostName Nothing
    bind sock sa
    listen sock 5
    port <- socketPort sock
    return (sock, fromIntegral port)

acceptSocket :: Socket -> IO Socket
acceptSocket sock = do
    (sock', _sa) <- accept sock
    setSocketOption sock' NoDelay 1
    return sock'

closeSock :: Socket -> IO ()
closeSock = close

createSocket :: BS.ByteString -> Maybe Word16 -> IO (Socket, SockAddr)
createSocket hostName portNumber = do
    ai <- addrInfo hostName portNumber
    sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
    return (sock, (addrAddress ai))

addrInfo :: BS.ByteString -> Maybe Word16 -> IO AddrInfo
addrInfo host port = do
    let hints = defaultHints { addrFlags = [ AI_CANONNAME, AI_NUMERICSERV, AI_ADDRCONFIG ]
                             , addrFamily = AF_INET
                             , addrSocketType = Stream
                             }
    (ai : _) <- getAddrInfo (Just hints) (Just (CS.unpack host)) (show <$> port)
    return ai
