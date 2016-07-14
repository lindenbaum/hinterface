module Language.Erlang.ControlMessage ( ControlMessage(..)
                                      , putControlMessage
                                      , getControlMessage
                                      )
       where

import Prelude hiding (length)
import Data.Binary
import Data.Binary.Get

import Util.Binary
import Language.Erlang.Term

--------------------------------------------------------------------------------

data ControlMessage = LINK Term Term          -- FromPid ToPid
                    | SEND Term Term          -- ToPid Message
                    | EXIT Term Term Term     -- FromPid ToPid Reason
                    | UNLINK Term Term        -- FromPid ToPid
                    | NODE_LINK               --
                    | REG_SEND Term Term Term -- FromPid ToName Message
                    | GROUP_LEADER Term Term  -- FromPid ToPid
                    | EXIT2 Term Term Term    -- FromPid ToPid Reason
                    deriving (Eq, Show)
--------------------------------------------------------------------------------

pass_through :: Word8
pass_through = 112

linkTag, sendTag, exitTag, unlinkTag, nodeLinkTag, regSendTag, groupLeaderTag, exit2Tag :: Term
linkTag = integer 1
sendTag = integer 2
exitTag = integer 3
unlinkTag = integer 4
nodeLinkTag = integer 5
regSendTag = integer 6
groupLeaderTag = integer 7
exit2Tag = integer 8

unused :: Term
unused = atom ""

--------------------------------------------------------------------------------

instance Binary ControlMessage where
  put (LINK fromPid toPid) = do
    putTerm $ tuple [linkTag, fromPid, toPid]

  put (SEND toPid message) = do
    putTerm $ tuple [sendTag, toPid]
    putTerm message

  put (EXIT fromPid toPid reason) = do
    putTerm $ tuple [exitTag, fromPid, toPid, reason]

  put (UNLINK fromPid toPid) = do
    putTerm $ tuple [unlinkTag, fromPid, toPid]

  put NODE_LINK = do
    putTerm $ tuple [nodeLinkTag]

  put (REG_SEND fromPid toName message) = do
    putTerm $ tuple [regSendTag, fromPid, unused, toName]
    putTerm message

  put (GROUP_LEADER fromPid toPid) = do
    putTerm $ tuple [groupLeaderTag, fromPid, toPid]

  put (EXIT2 fromPid toPid reason) = do
    putTerm $ tuple [exit2Tag, fromPid, toPid, reason]

  get = do
    term <- getTerm
    if is_tuple term
      then do get' term
      else do fail $ "Bad control message: " ++ show term
      where
        get' term
          | length term == 3 && element 1 term == linkTag = do
              return (LINK (element 2 term) (element 3 term))
          | length term == 3 && element 1 term == sendTag = do
              message <- getTerm
              return (SEND (element 3 term) message)
          | length term == 4 && element 1 term == exitTag = do
              return (EXIT (element 2 term) (element 3 term) (element 4 term))
          | length term == 3 && element 1 term == unlinkTag = do
              return (UNLINK (element 2 term) (element 3 term))
          | length term == 1 && element 1 term == nodeLinkTag = do
              return NODE_LINK
          | length term == 4 && element 1 term == regSendTag = do
              message <- getTerm
              return (REG_SEND (element 2 term) (element 4 term) message)
          | length term == 3 && element 1 term == groupLeaderTag = do
              return (GROUP_LEADER (element 2 term) (element 3 term))
          | length term == 4 && element 1 term == exit2Tag = do
              return (EXIT2 (element 2 term) (element 3 term) (element 4 term))
          | otherwise = do fail $ "Bad control message: " ++ show term

--------------------------------------------------------------------------------

putControlMessage :: ControlMessage -> Put
putControlMessage controlMessage = do
  putWithLength32be $ do
    putWord8 pass_through
    put controlMessage

--------------------------------------------------------------------------------

getControlMessage :: Get ControlMessage
getControlMessage = do
  expectedLen <- getWord32be
  pos0 <- bytesRead
  matchWord8 pass_through
  controlMessage <- get
  pos1 <- bytesRead
  let actualLen = pos1 - pos0
  if (fromIntegral expectedLen) == actualLen
    then do return controlMessage
    else do fail "Bad control message length"
