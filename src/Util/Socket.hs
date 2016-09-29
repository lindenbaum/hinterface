module Util.Socket
    ( connectSocket
    , serverSocket
    , acceptSocket
    , closeSock
    , Socket()
    ) where

import           Network.Socket        hiding ( recv, recvFrom, send, sendTo )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Word

import           Data.IOx

--------------------------------------------------------------------------------
connectSocket :: BS.ByteString -> Word16 -> IOx Socket
connectSocket = (toIOx .) . connectSocket'

connectSocket' :: BS.ByteString -> Word16 -> RawIO Socket
connectSocket' hostName portNumber = do
    (sock, sa) <- createSocket hostName (Just portNumber)
    setSocketOption sock NoDelay 1
    connect sock sa
    return sock

serverSocket :: BS.ByteString -> IOx (Socket, Word16)
serverSocket = toIOx . serverSocket'

serverSocket' :: BS.ByteString -> RawIO (Socket, Word16)
serverSocket' hostName = do
    (sock, sa) <- createSocket hostName Nothing
    bind sock sa
    listen sock 5
    port <- socketPort sock
    return (sock, fromIntegral port)

acceptSocket :: Socket -> IOx Socket
acceptSocket = toIOx . acceptSocket'

acceptSocket' :: Socket -> RawIO Socket
acceptSocket' sock = do
    (sock', sa) <- accept sock
    setSocketOption sock' NoDelay 1
    return sock'

closeSock :: Socket -> IOx ()
closeSock = toIOx . close

createSocket :: BS.ByteString -> Maybe Word16 -> RawIO (Socket, SockAddr)
createSocket hostName portNumber = do
    ai <- addrInfo hostName portNumber
    sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
    return (sock, (addrAddress ai))

addrInfo :: BS.ByteString -> Maybe Word16 -> RawIO AddrInfo
addrInfo host port = do
    let hints = defaultHints { addrFlags = [ AI_CANONNAME, AI_NUMERICSERV, AI_ADDRCONFIG ]
                             , addrFamily = AF_INET
                             , addrSocketType = Stream
                             }
    (ai : _) <- getAddrInfo (Just hints) (Just (CS.unpack host)) (show <$> port)
    return ai
