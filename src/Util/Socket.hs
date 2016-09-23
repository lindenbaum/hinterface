module Util.Socket ( connectSocket ) where

import           Network.Socket        hiding ( recv, recvFrom, send, sendTo )

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as CS
import           Data.Word

import           Util.IOx

--------------------------------------------------------------------------------
connectSocket :: BS.ByteString -> Word16 -> IOx Socket
connectSocket = (toIOx .) . connectSocket'

connectSocket' :: BS.ByteString -> Word16 -> RawIO Socket
connectSocket' hostName portNumber = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock NoDelay 1
    sa <- sockAddr hostName portNumber
    connect sock sa
    return sock
  where
    sockAddr :: BS.ByteString -> Word16 -> RawIO SockAddr
    sockAddr host port = do
        let hints = defaultHints { addrFlags = [ AI_CANONNAME, AI_NUMERICSERV, AI_ADDRCONFIG ]
                                 , addrFamily = AF_INET
                                 , addrSocketType = Stream
                                 }
        (ai : _) <- getAddrInfo (Just hints) (Just (CS.unpack host)) (Just (show port))
        return $ addrAddress ai
