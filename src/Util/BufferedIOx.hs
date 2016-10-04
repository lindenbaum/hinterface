-- FIXME rename module
module Util.BufferedIOx
    ( BufferedIOx(..)
    , runGetBuffered
    , runPutBuffered
    ) where

import qualified Data.ByteString      as BS ( ByteString )
import qualified Data.ByteString.Lazy as LBS ( ByteString )
import           Data.Binary
import           Util.Binary

import           Data.IOx

class BufferedIOx a where
    readBuffered :: a -> Int -> IOx BS.ByteString
    unreadBuffered :: a -> BS.ByteString -> IOx ()
    writeBuffered :: a -> LBS.ByteString -> IOx ()
    closeBuffered :: a -> IOx ()

runGetBuffered :: (BufferedIOx s, Binary a) => s -> IOx a
runGetBuffered = runGetSocket' get
  where
    runGetSocket' :: (BufferedIOx s) => Get a -> s -> IOx a
    runGetSocket' g s = (runGetSocket'' s g) >>= either (errorX userErrorType) (return)
      where
        runGetSocket'' :: (BufferedIOx s) => s -> Get a -> IOx (Either String a)
        runGetSocket'' = runGetA <$> readBuffered <*> unreadBuffered

runPutBuffered :: (BufferedIOx s, Binary a) => s -> a -> IOx ()
runPutBuffered = (. put) . runPutSocket'
  where
    runPutSocket' :: (BufferedIOx s) => s -> Put -> IOx ()
    runPutSocket' = runPutA <$> writeBuffered
