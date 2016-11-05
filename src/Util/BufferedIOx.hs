-- FIXME rename module
module Util.BufferedIOx
    ( BufferedIOx(..)
    , runGetBuffered
    , runPutBuffered
    , BufferedIOxException()
    ) where

import           Data.Binary
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)

import           Util.Binary
import           Util.IOExtra

class BufferedIOx a where
    readBuffered :: (MonadLoggerIO m) => a -> Int -> m BS.ByteString
    unreadBuffered :: (MonadLoggerIO m) => a -> BS.ByteString -> m ()
    writeBuffered :: (MonadLoggerIO m) => a -> LBS.ByteString -> m ()
    closeBuffered :: (MonadLoggerIO m) => a -> m ()

runGetBuffered :: (MonadLoggerIO m, BufferedIOx s, Binary a) => s -> m (Either BufferedIOxException a)
runGetBuffered s = do
    res <- runGetA (readBuffered s) (unreadBuffered s) get
    either (return . Left . BufferedIOxException) (return . Right) res

newtype BufferedIOxException = BufferedIOxException String
  deriving Show

instance Exception BufferedIOxException

runPutBuffered :: (MonadLoggerIO m, BufferedIOx s, Binary a) => s -> a -> m ()
runPutBuffered = ( . put) . runPutSocket'
  where
    runPutSocket' = runPutA <$> writeBuffered
