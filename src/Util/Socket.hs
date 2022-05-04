module Util.Socket
  ( connectSocket,
    serverSocket,
    acceptSocket,
    closeSock,
    Socket (),
  )
where

import Data.Word
import Network.Socket
import UnliftIO
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
connectSocket :: Text -> Word16 -> IO Socket
connectSocket hostName portNumber = do
  (sock, sa) <- createSocket hostName (Just portNumber)
  handleAny (\e -> closeSock sock >> throwIO e) $ do
    setSocketOption sock NoDelay 1
    connect sock sa
    return sock

serverSocket :: Text -> IO (Socket, Word16)
serverSocket hostName = do
  (sock, sa) <- createSocket hostName Nothing
  handleAny (\e -> closeSock sock >> throwIO e) $ do
    bind sock sa
    listen sock 5
    port <- socketPort sock
    return (sock, fromIntegral port)

acceptSocket :: Socket -> IO Socket
acceptSocket sock = do
  (sock', _sa) <- accept sock
  handleAny (\e -> closeSock sock' >> throwIO e) $ do
    setSocketOption sock' NoDelay 1
    return sock'

closeSock :: Socket -> IO ()
closeSock = close

createSocket ::
  MonadIO m =>
  Text ->
  Maybe Word16 ->
  m (Socket, SockAddr)
createSocket hostName portNumber =
  liftIO $ do
    ai <- addrInfo
    sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
    return (sock, addrAddress ai)
  where
    addrInfo = do
      let hints =
            defaultHints
              { addrFlags =
                  [ AI_CANONNAME,
                    AI_NUMERICSERV,
                    AI_ADDRCONFIG
                  ],
                addrFamily = AF_INET,
                addrSocketType = Stream
              }
      (ai : _) <-
        getAddrInfo
          (Just hints)
          (Just (Text.unpack hostName))
          (show <$> portNumber)
      return ai
