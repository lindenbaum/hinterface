{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}

-- | Internal module, exposed for unit tests
module Foreign.Erlang.LocalNode.Internal where

import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Reader
  ( MonadIO,
    ReaderT (..), asks
  )
import Data.Word ()
import Foreign.Erlang.Connection (Connection)
import Foreign.Erlang.Epmd (NodeRegistration)
import Foreign.Erlang.Handshake (HandshakeData)
import Foreign.Erlang.Mailbox (Mailbox)
import Foreign.Erlang.NodeData ()
import Foreign.Erlang.NodeState (NodeState)
import Foreign.Erlang.Term (Pid)
import Network.BufferedSocket ()
import UnliftIO (MonadUnliftIO)
import UnliftIO.Resource (MonadResource)
import Util.BufferedIOx ()
import Util.Socket (Socket)
import Prelude hiding (id)
import Data.Text (Text)

data LocalNode = LocalNode
  { handshakeData :: HandshakeData,
    nodeState :: NodeState Pid Text Mailbox Connection,
    acceptorSocket :: Socket
  }

data RegisteredNode = RegisteredNode
  { localNode :: LocalNode,
    nodeRegistration :: NodeRegistration
  }

data LocalNodeConfig = LocalNodeConfig
  { aliveName :: Text,
    hostName :: Text,
    cookie :: Text
  }
  deriving (Show)

newtype NodeT m a = NodeT {unNodeT :: ReaderT RegisteredNode m a}
  deriving (Functor, Applicative, Monad, MonadUnliftIO, MonadLogger, MonadIO)

deriving instance
  (MonadUnliftIO (NodeT m), MonadResource m) =>
  MonadResource (NodeT m)

deriving instance MonadLoggerIO m => MonadLoggerIO (NodeT m)

runRegisteredNode ::
  Monad m =>
  RegisteredNode ->
  NodeT m a ->
  m a
runRegisteredNode r = flip runReaderT r . unNodeT

askNodeState :: Monad m => NodeT m (NodeState Pid Text Mailbox Connection)
askNodeState = nodeState <$> askLocalNode

askLocalNode :: Monad m => NodeT m LocalNode
askLocalNode = NodeT (asks localNode)
