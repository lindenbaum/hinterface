-- FIXME rename module
module Util.BufferedIOx
    ( BufferedIOx(..)
    , runGetBuffered
    , runPutBuffered
    ) where

import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.ByteString        as BS ( ByteString )
import qualified Data.ByteString.Lazy   as LBS ( ByteString )
import           Util.Binary

import           Util.IOExtra

class BufferedIOx a where
    readBuffered :: (MonadIO m) => a -> Int -> m BS.ByteString
    unreadBuffered :: (MonadIO m) => a -> BS.ByteString -> m ()
    writeBuffered :: (MonadIO m) => a -> LBS.ByteString -> m ()
    closeBuffered :: (MonadIO m) => a -> m ()

runGetBuffered :: (BufferedIOx s, Binary a) => s -> IO a
runGetBuffered = runGetSocket' get
  where
    runGetSocket' :: (BufferedIOx s) => Get a -> s -> IO a
    runGetSocket' g s = (runGetSocket'' s g) >>= either (errorX userErrorType) (return)
      where
        runGetSocket'' :: (BufferedIOx s) => s -> Get a -> IO (Either String a)
        runGetSocket'' = runGetA <$> readBuffered <*> unreadBuffered

runPutBuffered :: (BufferedIOx s, Binary a) => s -> a -> IO ()
runPutBuffered = (. put) . runPutSocket'
  where
    runPutSocket' :: (BufferedIOx s) => s -> Put -> IO ()
    runPutSocket' = runPutA <$> writeBuffered
