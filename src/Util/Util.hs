module Util.Util
    ( runGetSocket2
    , runPutSocket2
    ) where

-- FIXME rename module
import           Data.Binary
import           Util.BufferedSocket
import           Util.Binary

import           Util.IOx

--------------------------------------------------------------------------------
runGetSocket :: BufferedSocket -> Get a -> IOx a
runGetSocket s g = (runGetSocket' s g) >>= either (errorX userErrorType) (return)

runGetSocket' :: BufferedSocket -> Get a -> IOx (Either String a)
runGetSocket' = runGetA <$> socketRecv <*> pushback

runGetSocket2 :: (Binary a) => BufferedSocket -> IOx a
runGetSocket2 = flip runGetSocket get

runPutSocket :: BufferedSocket -> Put -> IOx ()
runPutSocket = runPutA <$> socketSend

runPutSocket2 :: (Binary a) => BufferedSocket -> a -> IOx ()
runPutSocket2 = (. put) . runPutSocket
--------------------------------------------------------------------------------
