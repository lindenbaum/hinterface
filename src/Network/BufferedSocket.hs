module Network.BufferedSocket
    ( BufferedSocket()
    , makeBuffered
    , socketPort
    ) where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( MonadIO(..), liftIO )
import qualified Data.ByteString                as BS ( ByteString, append, empty, length, null, splitAt )
import qualified Data.ByteString.Lazy           as LBS ( ByteString )
import           Data.IORef                     ( IORef, atomicModifyIORef', newIORef, writeIORef )
import qualified Network.Socket                 as S ( PortNumber, Socket, close, socketPort )
import qualified Network.Socket.ByteString      as NBS ( recv )
import qualified Network.Socket.ByteString.Lazy as NBL ( sendAll )
import           Util.BufferedIOx

--------------------------------------------------------------------------------
newtype BufferedSocket = BufferedSocket (S.Socket, IORef BS.ByteString)

instance BufferedIOx BufferedSocket where
    readBuffered a = liftIO . socketRecv a
    unreadBuffered a = liftIO . pushback a
    writeBuffered a = liftIO . socketSend a
    closeBuffered = liftIO . socketClose

makeBuffered :: S.Socket -> IO BufferedSocket
makeBuffered sock = do
    bufIO <- newIORef BS.empty
    return $ BufferedSocket (sock, bufIO)

socketPort :: BufferedSocket -> IO S.PortNumber
socketPort (BufferedSocket (sock, _)) =
    S.socketPort sock

socketRecv :: BufferedSocket -> Int -> IO BS.ByteString
socketRecv (BufferedSocket (sock, bufIO)) len
    | len < 0 = error $ "Bad length: " ++ show len
    | len == 0 = return BS.empty
    | otherwise = do
          atomicModifyIORef' bufIO
                             (\buf -> if BS.null buf
                                      then (BS.empty, Nothing)
                                      else let bufLen = BS.length buf
                                           in
                                               if len > bufLen
                                               then (BS.empty, (Just buf))
                                               else let (buf0, buf1) = BS.splitAt len buf
                                                    in
                                                        (buf1, Just buf0)) >>=
              maybe (NBS.recv sock len) (return)

pushback :: BufferedSocket -> BS.ByteString -> IO ()
pushback (BufferedSocket (_, bufIO)) bytes = do
    unless (BS.null bytes) $ do
        atomicModifyIORef' bufIO (\buf -> (buf `BS.append` bytes, ()))

socketSend :: BufferedSocket -> LBS.ByteString -> IO ()
socketSend (BufferedSocket (sock, _)) bl = do
    NBL.sendAll sock bl

socketClose :: BufferedSocket -> IO ()
socketClose (BufferedSocket (sock, bufIO)) = do
    writeIORef bufIO BS.empty
    S.close sock
