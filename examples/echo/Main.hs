{-# LANGUAGE GADTs #-}
module Main where

import Foreign.Erlang.LocalNode
import Foreign.Erlang.Mailbox
import Foreign.Erlang.Term
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad
import Control.Monad.IO.Class
import Data.String
import Control.Exception.Lifted        ( SomeException, catch )

main :: IO ()
main =
    runResourceT
        $ runStdoutLoggingT
        $ runNodeT (LocalNodeConfig "hinterface-echo" "localhost" "cookie")
        $ do
              mailbox <- make_mailbox
              register_pid "echo" (self mailbox)
              forever
                  (       ( do m <- liftIO $ receive mailbox
                               logInfoN (fromString (show m))
                               return ()
                          )
                  `catch` ( logErrorN
                          . fromString
                          . (show :: SomeException -> String)
                          )
                  )

