module Network.BufferedSocket
    ( BufferedSocket()
    , makeBuffered
    , socketPort
    ) where

import           Control.Monad                  (unless)
import           Control.Monad.IO.Class         (MonadIO (..), liftIO)
import qualified Data.ByteString                as BS (ByteString, append,
                                                       empty, length, null,
                                                       splitAt)
import qualified Data.ByteString.Lazy           as LBS (ByteString)
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 newIORef, writeIORef)
import qualified Network.Socket                 as S (PortNumber, Socket, close,
                                                      socketPort)
import qualified Network.Socket.ByteString      as NBS (recv)

import qualified Network.Socket.ByteString.Lazy as NBL (sendAll)
import           Util.BufferedIOx
import           Util.IOExtra

--------------------------------------------------------------------------------
newtype BufferedSocket = BufferedSocket (S.Socket, IORef BS.ByteString)

instance BufferedIOx BufferedSocket where
    readBuffered = socketRecv
    unreadBuffered = pushback
    writeBuffered = socketSend
    closeBuffered = socketClose

makeBuffered :: (MonadLoggerIO m) => S.Socket -> m BufferedSocket
makeBuffered sock = do
    bufIO <- liftIO $ newIORef BS.empty
    return $ BufferedSocket (sock, bufIO)

socketPort :: (MonadLoggerIO m) => BufferedSocket -> m S.PortNumber
socketPort (BufferedSocket (sock, _)) =
    liftIO $ S.socketPort sock

socketRecv :: (MonadLoggerIO m) => BufferedSocket -> Int -> m BS.ByteString
socketRecv (BufferedSocket (sock, bufIO)) len
    | len < 0 = error $ "Bad length: " ++ show len
    | len == 0 = return BS.empty
    | otherwise = liftIO $
          atomicModifyIORef' bufIO
                             (\buf -> if BS.null buf
                                      then (BS.empty, Nothing)
                                      else let bufLen = BS.length buf
                                           in
                                               if len > bufLen
                                               then (BS.empty, Just buf)
                                               else let (buf0, buf1) = BS.splitAt len
                                                                                  buf
                                                    in
                                                        (buf1, Just buf0)) >>=
              maybe (NBS.recv sock len) return

pushback :: (MonadLoggerIO m) => BufferedSocket -> BS.ByteString -> m ()
pushback (BufferedSocket (_, bufIO)) bytes =
    unless (BS.null bytes) $
        liftIO $
            atomicModifyIORef' bufIO (\buf -> (buf `BS.append` bytes, ()))

socketSend :: (MonadLoggerIO m) => BufferedSocket -> LBS.ByteString -> m ()
socketSend (BufferedSocket (sock, _)) bl =
    liftIO $
        NBL.sendAll sock bl

socketClose :: (MonadLoggerIO m) => BufferedSocket -> m ()
socketClose (BufferedSocket (sock, bufIO)) =
    liftIO $ do
        writeIORef bufIO BS.empty
        S.close sock
