{-# LANGUAGE RankNTypes #-}
module Util.IOExtra
    ( errorX
    , maybeErrorX
    , logX
    , module X
    ) where

import           Control.Concurrent as X
import           Control.Exception  as X
import           System.IO.Error    as X

--------------------------------------------------------------------------------

errorX :: IOErrorType -> String -> IO a
errorX errorType location =
    throw $ mkIOError errorType location Nothing Nothing

maybeErrorX :: IOErrorType -> String -> Maybe a -> IO a
maybeErrorX errorType location =
    maybe (errorX errorType location) (return)

logX :: String -> IOError -> IO ()
logX msg x = do
    putStr msg
    putStr ": "
    print x
