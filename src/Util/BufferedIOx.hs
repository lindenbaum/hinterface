-- FIXME rename module
module Util.BufferedIOx
    ( BufferedIOx(..)
    , runGetBuffered
    , runPutBuffered
    , module Util.Binary
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

runGetBuffered :: (MonadIO m, BufferedIOx s, Binary a, MonadMask m, MonadLogger m) => s -> m a
runGetBuffered s =
  throwLeftM (runGetA (liftIO . readBuffered s) (liftIO . unreadBuffered s) get)

runPutBuffered :: (MonadIO m, BufferedIOx s, Binary a) => s -> a -> m ()
runPutBuffered s = runPutA (liftIO . writeBuffered s) . put
