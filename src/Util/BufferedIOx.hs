-- FIXME rename module
module Util.BufferedIOx
  ( BufferedIOx (..),
    runGetBuffered,
    runPutBuffered,
    module Util.Binary,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Binary
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import UnliftIO
import Util.Binary
import Util.IOExtra

class BufferedIOx a where
  readBuffered :: (MonadIO m) => a -> Int -> m BS.ByteString
  unreadBuffered :: (MonadIO m) => a -> BS.ByteString -> m ()
  writeBuffered :: (MonadIO m) => a -> LBS.ByteString -> m ()
  closeBuffered :: (MonadIO m) => a -> m ()

runGetBuffered ::
  ( MonadIO m,
    BufferedIOx s,
    Binary a,
    MonadUnliftIO m,
    MonadLoggerIO m
  ) =>
  s ->
  m a
runGetBuffered s =
  throwLeftM (runGetA (liftIO . readBuffered s) (liftIO . unreadBuffered s) get)

runPutBuffered :: (MonadIO m, BufferedIOx s, Binary a) => s -> a -> m ()
runPutBuffered s = runPutA (liftIO . writeBuffered s) . put
