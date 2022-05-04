{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foreign.Erlang.LocalNodeSpec (spec) where

import Control.Monad.Logger (runStdoutLoggingT)
import Foreign.Erlang.Connection (Connection)
import Foreign.Erlang.Epmd (mkTestingNodeRegistration)
import Foreign.Erlang.Handshake
  ( HandshakeData (HandshakeData, cookie, name, nodeData),
    otp23Name,
  )
import Foreign.Erlang.LocalNode (make_pid, make_port, make_ref, register_pid, make_mailbox)
import Foreign.Erlang.LocalNode.Internal
  ( LocalNode (LocalNode, acceptorSocket, handshakeData, nodeState),
    RegisteredNode (RegisteredNode),
    runRegisteredNode,
  )
import Foreign.Erlang.Mailbox (Mailbox (self))
import Foreign.Erlang.NodeData
  ( DistributionVersion (R6B, Zero),
    NodeData
      ( NodeData,
        aliveName,
        extra,
        hiVer,
        loVer,
        nodeType,
        portNo,
        protocol
      ),
    NodeProtocol (TcpIpV4),
    NodeType (HiddenNode),
  )
import Foreign.Erlang.NodeState (NodeState, getMailboxForName, getMailboxForPid, newNodeState)
import Foreign.Erlang.Term (Pid (MkPid), Term (NewPid, NewPort, NewerReference))
import Test.Hspec (Spec, describe, it, shouldNotBe, shouldSatisfy)
import Data.Text (Text)
import Control.Monad.Trans.Resource (runResourceT)

spec :: Spec
spec = do
  describe "LocalNode" $ do
    it "make_pid, make_port and make_ref return terms with big-creations" $ do
      (_, r) <- mkTestingRegisteredNode
      (MkPid p, o, f) <-
        runStdoutLoggingT $
          runRegisteredNode r $
            (,,)
              <$> make_pid
              <*> make_port
              <*> make_ref
      p `shouldSatisfy` \case
        NewPid {} -> True
        _ -> False
      o `shouldSatisfy` \case
        NewPort {} -> True
        _ -> False
      f `shouldSatisfy` \case
        NewerReference {} -> True
        _ -> False

    it "consequtive calls to make_pid return different values" $ do
      (_, r) <- mkTestingRegisteredNode
      (p1, p2) <- runStdoutLoggingT $ runRegisteredNode r $ (,) <$> make_pid <*> make_pid
      p1 `shouldNotBe` p2
    it "after make_pid a mailbox can be retrieved with the returned pid" $ do
      (nodeSt, r) <- mkTestingRegisteredNode
      mb <-
        runStdoutLoggingT $ runResourceT $ runRegisteredNode
            r
            (make_mailbox >>= getMailboxForPid nodeSt . self)
      (() <$ mb) `shouldNotBe` Nothing
    it "a registered pid can be retreived" $ do
      (nodeSt, r) <- mkTestingRegisteredNode
      retreivedMailbox <- runStdoutLoggingT $
        runRegisteredNode r $ do
          let name = "test"
          mb <- make_mailbox
          register_pid name (self mb)
          getMailboxForName nodeSt name
      (() <$ retreivedMailbox) `shouldNotBe` Nothing

mkTestingRegisteredNode ::
  IO
    ( NodeState
        Pid
        Text
        Mailbox
        Foreign.Erlang.Connection.Connection,
      RegisteredNode
    )
mkTestingRegisteredNode = do
  nodeSt <- newNodeState
  return
    ( nodeSt,
      RegisteredNode
        LocalNode
          { handshakeData =
              HandshakeData
                { name = otp23Name "test-node-name@127.0.0.1",
                  nodeData =
                    NodeData
                      { portNo = 12345,
                        nodeType = HiddenNode,
                        protocol = TcpIpV4,
                        hiVer = R6B,
                        loVer = Zero,
                        Foreign.Erlang.NodeData.aliveName = "test-node-name",
                        extra = "test-extra"
                      },
                  Foreign.Erlang.Handshake.cookie = "test-cookie"
                },
            nodeState = nodeSt,
            acceptorSocket = error "acceptorSocket no available in unit test"
          }
        (mkTestingNodeRegistration 777)
    )