module Util.Util ( runGetSocket
                 , runPutSocket
                 )
       where
 -- FIXME rename module

import Data.Binary.Get
import Data.Binary.Put

import Util.BufferedSocket
import Util.Binary

import Util.IOx

--------------------------------------------------------------------------------

runGetSocket :: BufferedSocket -> Get a -> IOx a
runGetSocket s g = (runGetSocket' s g) >>= either (errorX userErrorType) (return)

runGetSocket' :: BufferedSocket -> Get a  -> IOx (Either String a)
runGetSocket' = runGetA <$> socketRecv <*> pushback

runPutSocket :: BufferedSocket -> Put -> IOx ()
runPutSocket = runPutA <$> socketSend

--------------------------------------------------------------------------------
