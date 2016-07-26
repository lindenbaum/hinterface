module Util.IOx ( RawIO
                , IOx
                , errorX
                , maybeErrorX
                , catchX
                , toIOx
                , fromIOx
                , liftIOx
                , forkIOx
                , killThreadX
                , atomicallyX
                , printX
                , logX
                , doesNotExistErrorType
                , alreadyExistsErrorType
                , illegalOperationErrorType
                , userErrorType
                )
       where

import System.IO.Error

import Control.Monad.Trans
import Control.Monad.Trans.Either

import Control.Concurrent
import Control.Concurrent.STM

--------------------------------------------------------------------------------

type RawIO = IO

type IOx = EitherT IOError RawIO

errorX :: IOErrorType -> String -> IOx a
errorX errorType location = left $ mkIOError errorType location Nothing Nothing

maybeErrorX :: IOErrorType -> String -> Maybe a -> IOx a
maybeErrorX errorType location = maybe (errorX errorType location) (return)

catchX :: IOx a -> (IOError -> IOx a) -> IOx a
ma `catchX` handler = mapEitherT (>>= either (runEitherT . handler) (return . Right)) ma

toIOx :: RawIO a -> IOx a
toIOx = EitherT . tryIOError . liftIOx

fromIOx :: IOx a -> RawIO a
fromIOx ma = runEitherT ma >>= either (error . ("ERROR: " ++) . show) (return)

liftIOx :: (MonadIO m) => IO a -> m a
liftIOx = liftIO

forkIOx :: IOx () -> IOx ThreadId
forkIOx = toIOx . forkIO . fromIOx -- FIXME check how/when IOError kills thread

killThreadX :: ThreadId -> IOx ()
killThreadX = toIOx . killThread

atomicallyX :: STM a -> IOx a
atomicallyX = toIOx . atomically

printX :: (Show a) => a -> IOx ()
printX = liftIOx . print

logX :: String -> IOError -> IOx ()
logX msg x = toIOx $ do
  putStr msg
  putStr ": "
  print x
