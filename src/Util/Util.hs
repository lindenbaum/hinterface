-- FIXME rename module
module Util.Util
    ( runGetSocket
    , runPutSocket
    ) where

import           Data.Binary
import           Network.BufferedSocket
import           Util.Binary

import           Data.IOx

runGetSocket'' :: BufferedSocket -> Get a -> IOx (Either String a)
runGetSocket'' = runGetA <$> socketRecv <*> pushback

runGetSocket' :: BufferedSocket -> Get a -> IOx a
runGetSocket' s g = (runGetSocket'' s g) >>= either (errorX userErrorType) (return)

runGetSocket :: (Binary a) => BufferedSocket -> IOx a
runGetSocket = flip runGetSocket' get

runPutSocket' :: BufferedSocket -> Put -> IOx ()
runPutSocket' = runPutA <$> socketSend

runPutSocket :: (Binary a) => BufferedSocket -> a -> IOx ()
runPutSocket = (. put) . runPutSocket'
