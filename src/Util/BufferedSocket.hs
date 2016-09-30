module Util.BufferedSocket
    ( BufferedSocket()
    , makeBuffered
    , socketPort
    , socketRecv
    , pushback
    , socketSend
    , socketClose
    ) where

import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( liftIO )

import qualified Data.ByteString                as BS ( ByteString, append, empty, length, null, splitAt )
import qualified Data.ByteString.Lazy           as BL ( ByteString )

import qualified Network.Socket                 as S ( PortNumber, Socket, close, socketPort )
import qualified Network.Socket.ByteString      as NBS ( recv )
import qualified Network.Socket.ByteString.Lazy as NBL ( sendAll )

import           Data.IORef                     ( IORef, modifyIORef', newIORef, readIORef, writeIORef )

import           Data.IOx

--------------------------------------------------------------------------------
newtype BufferedSocket = BufferedSocket (S.Socket, IORef BS.ByteString)

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
          buf <- readIORef bufIO
          if BS.null buf
              then do
                  NBS.recv sock len
              else do
                  let bufLen = BS.length buf
                  if len > bufLen
                      then do
                          writeIORef bufIO BS.empty
                          return buf
                      else do
                          let (buf0, buf1) = BS.splitAt len buf
                          writeIORef bufIO buf1
                          return buf0

pushback :: BufferedSocket -> BS.ByteString -> IOx ()
pushback (BufferedSocket (_, bufIO)) buf0 = do
    unless (BS.null buf0) $
        toIOx $ do
            modifyIORef' bufIO $ BS.append buf0

socketSend :: BufferedSocket -> BL.ByteString -> IOx ()
socketSend (BufferedSocket (sock, _)) bl =
    toIOx $ do
        NBL.sendAll sock bl

socketClose :: BufferedSocket -> IOx ()
socketClose (BufferedSocket (sock, bufIO)) =
    toIOx $ do
        writeIORef bufIO BS.empty
        S.close sock
