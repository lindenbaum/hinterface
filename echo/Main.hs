{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main (main) where

import Control.Monad (forever)
import Control.Monad.Logger
import Data.String
import Foreign.Erlang.LocalNode
import Foreign.Erlang.Mailbox
import Foreign.Erlang.Term
import UnliftIO (MonadIO (liftIO), MonadUnliftIO)
import UnliftIO.Exception
import UnliftIO.Resource
import Util.IOExtra (logInfoStr)
import qualified Data.Text.IO as T
import Data.Text (Text)


main :: IO ()
main = do
  let cfg =
        LocalNodeConfig
          { aliveName = "acceptor",
            hostName = "127.0.0.1",
            cookie = "cookie"
          }
      serverName = "echo_server"
  T.putStrLn "This program requires an Erlang node to connect to.\n"
  T.putStrLn "To start a node run:"
  T.putStrLn ("    erl -setcookie " <> cookie cfg <> " -name client@" <> hostName cfg <> "\n")
  T.putStrLn "Then connect to the echo node (this program) by typing into the Erlang shee:"
  T.putStrLn ("    net_kernel:connect_node('" <> aliveName cfg <> "@" <> hostName cfg <> "').\n")
  T.putStrLn "Now you can send messages like this:"
  T.putStrLn ("    {" <> serverName <> ", '" <> aliveName cfg <> "@" <> hostName cfg <> "'} ! test.\n")
  T.putStrLn "... or if you want the message to be echoed back:"
  T.putStrLn ("    {" <> serverName <> ", '" <> aliveName cfg <> "@" <> hostName cfg <> "'} ! {self(), test}.\n")
  T.putStrLn "To receive echoed message run:"
  T.putStrLn "    f(X), receive X -> io:format(\"GOT ~w~n\", [X]) after 100 -> io:format(\"got nothing~n\") end.\n\n"
  runResourceT $
    runStdoutLoggingT $
      runNodeT cfg $ do
        mailbox <- make_mailbox
        register_pid serverName (self mailbox)
        forever
          ( processCommand serverName mailbox
              `catch` ( logErrorN
                          . fromString
                          . (show :: SomeException -> String)
                      )
          )

processCommand :: (MonadLoggerIO m, MonadResource m, MonadUnliftIO m) => Text -> Mailbox -> NodeT m ()
processCommand serverName mailbox = do
  logInfoN ("process '" <> serverName <> "' is waiting for an incoming message")
  msgIn <- liftIO (receive mailbox)
  logInfoStr ("received: " ++ show msgIn)
  case msgIn of
    Tuple2 from payload -> do
      logInfoStr ("received: " ++ show payload)
      send (MkPid from) payload
      logInfoStr ("sent message back to " ++ show from)
    _ ->
      logInfoStr ("received: " ++ show msgIn)
