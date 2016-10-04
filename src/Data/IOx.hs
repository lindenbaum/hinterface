module Data.IOx
    ( RawIO
    , IOx
    , errorX
    , maybeErrorX
    , throwX
    , catchX
    , toIOx
    , fromIOx
    , forkIOx
    , killThreadX
    , logX
    , doesNotExistErrorType
    , alreadyExistsErrorType
    , illegalOperationErrorType
    , userErrorType
    ) where

import           System.IO.Error

import           Control.Monad.Trans
import           Control.Monad.Trans.Either

import           Control.Concurrent

--------------------------------------------------------------------------------
type RawIO = IO

type IOx = EitherT IOError RawIO

errorX :: IOErrorType -> String -> IOx a
errorX errorType location =
    throwX $ mkIOError errorType location Nothing Nothing

maybeErrorX :: IOErrorType -> String -> Maybe a -> IOx a
maybeErrorX errorType location =
    maybe (errorX errorType location) (return)

throwX :: IOError -> IOx a
throwX = left

catchX :: IOx a -> (IOError -> IOx a) -> IOx a
ma `catchX` handler = mapEitherT (>>= either (runEitherT . handler) (return . Right)) ma

toIOx :: RawIO a -> IOx a
toIOx = EitherT . tryIOError . liftIO

fromIOx :: IOx a -> RawIO a
fromIOx ma = runEitherT ma >>= either (error . ("IOx ERROR: " ++) . show) (return)

forkIOx :: IOx () -> IOx ThreadId
forkIOx = toIOx . forkIO . fromIOx -- FIXME check how/when IOError kills thread

killThreadX :: ThreadId -> IOx ()
killThreadX = toIOx . killThread

logX :: String -> IOError -> IOx ()
logX msg x = toIOx $ do
    putStr msg
    putStr ": "
    print x
