-- FIXME rename module
module Util.BufferedIOx
    ( BufferedIOx(..)
    , runGetBuffered
    , runPutBuffered
    ) where

import           Data.Binary
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import           Util.Binary

import           Util.IOExtra

class BufferedIOx a where
    readBuffered :: a -> Int -> IO BS.ByteString
    unreadBuffered :: a -> BS.ByteString -> IO ()
    writeBuffered :: a -> LBS.ByteString -> IO ()
    closeBuffered :: a -> IO ()

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
