{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

module Foreign.Erlang.NodeState
  ( NodeState (),
    logNodeState,
    newNodeState,
    new_pid,
    new_port,
    new_ref,
    putMailboxForPid,
    getMailboxForPid,
    putMailboxForName,
    getMailboxForName,
    putConnectionForNode,
    getConnectionForNode,
    removeConnectionForNode,
    getConnectedNodes,
  )
where

import Control.Monad (void, when)
import Control.Monad.Logger
import qualified Data.Map.Strict as M
import Data.Word
import Text.Printf (printf)
import UnliftIO
import Util.IOExtra (logInfoStr)

-- import           Util.IOExtra

--------------------------------------------------------------------------------
data NodeState p n mb c = NodeState
  { serial :: TVar Word32,
    pidId :: TVar Word32,
    portId :: TVar Word32,
    refId0 :: TVar Word32,
    refId1 :: TVar Word32,
    refId2 :: TVar Word32,
    pid2Mbox :: TVar (M.Map p mb),
    name2Mbox :: TVar (M.Map n mb),
    node2Conn :: TVar (M.Map n c)
  }

instance Show (NodeState p n mb c) where
  show _ = "#NodeState<>"

--------------------------------------------------------------------------------

newNodeState :: IO (NodeState p n mb c)
newNodeState =
  NodeState <$> newTVarIO 0
    <*> newTVarIO 1 --  serial
    <*> newTVarIO 1 --  pidId
    <*> newTVarIO 0 --  portId
    <*> newTVarIO 0 --  refId0
    <*> newTVarIO 0 --  refId1
    <*> newTVarIO M.empty --  refId2
    <*> newTVarIO M.empty --  pid2Mbox
    <*> newTVarIO M.empty --  name2MBox
    --  name2Conn

logNodeState :: (Show n, MonadIO m, MonadLogger m) => NodeState p n mb c -> m ()
logNodeState NodeState {node2Conn} =
  do
    m <- liftIO (readTVarIO node2Conn)
    logInfoStr (printf "known connection keys %s" (unlines (show <$> M.keys m)))

--------------------------------------------------------------------------------
new_pid :: NodeState p n mb c -> IO (Word32, Word32)
new_pid NodeState {serial, pidId} =
  atomically $ do
    let p = (,) <$> readTVar pidId <*> readTVar serial

    whenM (inc pidId _15bits) $
      void (inc serial _13bits)

    p

--------------------------------------------------------------------------------
new_port :: NodeState p n mb c -> IO Word32
new_port NodeState {portId} =
  atomically $ do
    let p = readTVar portId

    void (inc portId _28bits)

    p

--------------------------------------------------------------------------------
new_ref :: NodeState p n mb c -> IO (Word32, Word32, Word32)
new_ref NodeState {refId0, refId1, refId2} =
  atomically $ do
    let r = (,,) <$> readTVar refId0 <*> readTVar refId1 <*> readTVar refId2

    whenM (inc refId0 _18bits) $
      whenM (inc refId1 _32bits) $
        void (inc refId2 _32bits)

    r

--------------------------------------------------------------------------------
putMailboxForPid :: (Ord p) => NodeState p n mb c -> p -> mb -> IO ()
putMailboxForPid NodeState {pid2Mbox} pid mbox =
  atomically $ modifyTVar' pid2Mbox (M.insert pid mbox)

getMailboxForPid :: (Ord p) => NodeState p n mb c -> p -> IO (Maybe mb)
getMailboxForPid NodeState {pid2Mbox} pid = M.lookup pid <$> readTVarIO pid2Mbox

--------------------------------------------------------------------------------
putMailboxForName :: (Ord n) => NodeState p n mb c -> n -> mb -> IO ()
putMailboxForName NodeState {name2Mbox} name mbox =
  atomically $ modifyTVar' name2Mbox (M.insert name mbox)

getMailboxForName :: (Ord n) => NodeState p n mb c -> n -> IO (Maybe mb)
getMailboxForName NodeState {name2Mbox} name =
  M.lookup name <$> readTVarIO name2Mbox

--------------------------------------------------------------------------------
putConnectionForNode :: (Ord n) => NodeState p n mb c -> n -> c -> IO ()
putConnectionForNode NodeState {node2Conn} name conn =
  atomically $ modifyTVar' node2Conn (M.insert name conn)

getConnectionForNode :: (MonadIO m, Ord n) => NodeState p n mb c -> n -> m (Maybe c)
getConnectionForNode NodeState {node2Conn} name =
  M.lookup name <$> liftIO (readTVarIO node2Conn)

removeConnectionForNode :: (Ord n) => NodeState p n mb c -> n -> IO ()
removeConnectionForNode NodeState {node2Conn} name =
  atomically $ modifyTVar' node2Conn (M.delete name)

getConnectedNodes :: NodeState p n mb c -> IO [(n, c)]
getConnectedNodes NodeState {node2Conn} =
  M.toList <$> readTVarIO node2Conn

--------------------------------------------------------------------------------
_13bits, _15bits, _18bits, _28bits, _32bits :: Word32
_13bits = 0x00001fff
_15bits = 0x00007fff
_18bits = 0x0003ffff
_28bits = 0x0fffffff
_32bits = 0xffffffff

inc :: TVar Word32 -> Word32 -> STM Bool
inc tV maxV = do
  modifyTVar' tV (+ 1)
  v <- readTVar tV
  if v > maxV
    then do
      writeTVar tV 0
      return True
    else do
      return False

whenM :: Monad m => m Bool -> m () -> m ()
whenM mt mc = do
  t <- mt
  when t mc
