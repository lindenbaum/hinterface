{-# LANGUAGE Strict #-}

module Foreign.Erlang.NodeState
    ( NodeState()
    , newNodeState
    , new_pid
    , new_port
    , new_ref
    , putMailboxForPid
    , getMailboxForPid
    , putMailboxForName
    , getMailboxForName
    , putConnectionForNode
    , getConnectionForNode
    , removeConnectionForNode
    , getConnectedNodes
    ) where

import           Control.Monad          ( when )
import           Control.Monad.IO.Class
import           Control.Concurrent.STM

import           Data.Word
import qualified Data.Map.Strict        as M

import           Util.IOExtra

--------------------------------------------------------------------------------
data NodeState p n mb c =
      NodeState { serial    :: TVar Word32
                , pidId     :: TVar Word32
                , portId    :: TVar Word32
                , refId0    :: TVar Word32
                , refId1    :: TVar Word32
                , refId2    :: TVar Word32
                , pid2Mbox  :: TVar (M.Map p mb)
                , name2Mbox :: TVar (M.Map n mb)
                , node2Conn :: TVar (M.Map n c)
                }

instance Show (NodeState p n mb c) where
    show _ = "#NodeState<>"

--------------------------------------------------------------------------------
newNodeState :: MonadIO m => m (NodeState p n mb c)
newNodeState = liftIO $
    NodeState <$> newTVarIO 0
              <*>  --  serial
               newTVarIO 1
              <*>  --  pidId
               newTVarIO 1
              <*>  --  portId
               newTVarIO 0
              <*>  --  refId0
               newTVarIO 0
              <*>  --  refId1
               newTVarIO 0
              <*>  --  refId2
               newTVarIO M.empty
              <*>  --  pid2Mbox
               newTVarIO M.empty
              <*>  --  name2MBox
               newTVarIO M.empty      --  name2Conn

--------------------------------------------------------------------------------
new_pid :: (MonadIO m) => NodeState p n mb c -> m (Word32, Word32)
new_pid NodeState{serial,pidId} =
    liftIO $
        atomically $ do
            let p = (,) <$> readTVar pidId <*> readTVar serial

            whenM (inc pidId _15bits) $
                voidM (inc serial _13bits)

            p

--------------------------------------------------------------------------------
new_port :: (MonadIO m) => NodeState p n mb c -> m Word32
new_port NodeState{portId} =
    liftIO $
        atomically $ do
            let p = readTVar portId

            voidM (inc portId _28bits)

            p

--------------------------------------------------------------------------------
new_ref :: (MonadIO m) => NodeState p n mb c -> m (Word32, Word32, Word32)
new_ref NodeState{refId0,refId1,refId2} =
    liftIO $
        atomically $ do
            let r = (,,) <$> readTVar refId0 <*> readTVar refId1 <*> readTVar refId2

            whenM (inc refId0 _18bits) $
                whenM (inc refId1 _32bits) $
                    voidM (inc refId2 _32bits)

            r

--------------------------------------------------------------------------------
putMailboxForPid :: (MonadIO m, Ord p) => NodeState p n mb c -> p -> mb -> m ()
putMailboxForPid NodeState{pid2Mbox} pid mbox =
    liftIO $
        atomically $ do
            modifyTVar' pid2Mbox (M.insert pid mbox)

getMailboxForPid :: (MonadIO m, Ord p, Show p) => NodeState p n mb c -> p -> m mb
getMailboxForPid NodeState{pid2Mbox} pid = do
    mb <- liftIO $ atomically $ readTVar pid2Mbox
    maybeErrorX doesNotExistErrorType (show pid) (M.lookup pid mb)

--------------------------------------------------------------------------------
putMailboxForName :: (MonadIO m, Ord n) => NodeState p n mb c -> n -> mb -> m ()
putMailboxForName NodeState{name2Mbox} name mbox = do
    liftIO $ atomically $ modifyTVar' name2Mbox (M.insert name mbox)

getMailboxForName :: (MonadIO m, Ord n, Show n) => NodeState p n mb c -> n -> m mb
getMailboxForName NodeState{name2Mbox} name = do
    mb <- liftIO $ atomically $ readTVar name2Mbox
    maybeErrorX doesNotExistErrorType (show name) (M.lookup name mb)

--------------------------------------------------------------------------------
putConnectionForNode :: (MonadIO m, Ord n) => NodeState p n mb c -> n -> c -> m ()
putConnectionForNode NodeState{node2Conn} name conn = do
    liftIO $ atomically $ modifyTVar' node2Conn (M.insert name conn)

getConnectionForNode :: (MonadIO m, Ord n, Show n) => NodeState p n mb c -> n -> m c
getConnectionForNode NodeState{node2Conn} name = do
    mb <- liftIO $ atomically $ readTVar node2Conn
    maybeErrorX doesNotExistErrorType (show name) (M.lookup name mb)

removeConnectionForNode :: (MonadIO m, Ord n) => NodeState p n mb c -> n -> m ()
removeConnectionForNode NodeState{node2Conn} name = do
    liftIO $ atomically $ modifyTVar' node2Conn (M.delete name)

getConnectedNodes :: (MonadIO m) => NodeState p n mb c -> m [(n, c)]
getConnectedNodes NodeState{node2Conn} = do
    mb <- liftIO $ atomically $ readTVar node2Conn
    return $ M.toList mb

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

--------------------------------------------------------------------------------
whenM :: Monad m => m Bool -> m () -> m ()
whenM mt mc = do
    t <- mt
    when t mc

voidM :: Monad m => m a -> m ()
voidM ma = do
    _ <- ma
    return ()--------------------------------------------------------------------------------
