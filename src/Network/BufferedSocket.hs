module Network.BufferedSocket
    ( BufferedSocket()
    , makeBuffered
    , socketPort
    ) where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.ByteString                as BS ( ByteString, append, empty, length, null, splitAt )
import qualified Data.ByteString.Lazy           as LBS ( ByteString )

import qualified Network.Socket                 as S ( PortNumber, Socket, close, socketPort )
import qualified Network.Socket.ByteString      as NBS ( recv )
import qualified Network.Socket.ByteString.Lazy as NBL ( sendAll )

import           Data.IORef                     ( IORef, atomicModifyIORef', newIORef, writeIORef )

import           Data.IOx
import           Util.BufferedIOx

--------------------------------------------------------------------------------
newtype BufferedSocket = BufferedSocket (S.Socket, IORef BS.ByteString)

instance BufferedIOx BufferedSocket where
    readBuffered = socketRecv
    unreadBuffered = pushback
    writeBuffered = socketSend
    closeBuffered = socketClose

makeBuffered :: S.Socket -> IOx BufferedSocket
makeBuffered sock = do
    bufIO <- liftIO $ newIORef BS.empty
    return $ BufferedSocket (sock, bufIO)

socketPort :: BufferedSocket -> IOx S.PortNumber
socketPort (BufferedSocket (sock, _)) =
    toIOx $ do
        S.socketPort sock

socketRecv :: BufferedSocket -> Int -> IOx BS.ByteString
socketRecv (BufferedSocket (sock, bufIO)) len
    | len < 0 = error $ "Bad length: " ++ show len
    | len == 0 = return BS.empty
    | otherwise = toIOx $ do
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

pushback :: BufferedSocket -> BS.ByteString -> IOx ()
pushback (BufferedSocket (_, bufIO)) bytes = do
    unless (BS.null bytes) $
        toIOx $ do
            atomicModifyIORef' bufIO (\buf -> (buf `BS.append` bytes, ()))

socketSend :: BufferedSocket -> LBS.ByteString -> IOx ()
socketSend (BufferedSocket (sock, _)) bl =
    toIOx $ do
        NBL.sendAll sock bl

socketClose :: BufferedSocket -> IOx ()
socketClose (BufferedSocket (sock, bufIO)) =
    toIOx $ do
        writeIORef bufIO BS.empty
        S.close sock
