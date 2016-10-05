{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Erlang.ControlMessage ( ControlMessage(..) ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Maybe
import           Language.Erlang.Term
import           Prelude                   hiding (length)
import           Test.QuickCheck
import           Util.Binary

--------------------------------------------------------------------------------
data ControlMessage = TICK
                    | LINK Pid Pid           -- FromPid ToPid
                    | SEND Pid Term          -- ToPid Message
                    | EXIT Pid Pid Term      -- FromPid ToPid Reason
                    | UNLINK Pid Pid         -- FromPid ToPid
                    | NODE_LINK              --
                    | REG_SEND Pid Term Term -- FromPid ToName Message
                    | GROUP_LEADER Pid Pid   -- FromPid ToPid
                    | EXIT2 Pid Pid Term     -- FromPid ToPid Reason
    deriving (Eq, Show)

instance Binary ControlMessage where
    put TICK = putWord32be 0
    put controlMessage = putWithLength32be $ do
        putWord8 pass_through
        put' controlMessage
      where
        put' TICK = fail "Unreachable code"

        put' (LINK fromPid toPid) = do
            putTerm (linkTag, fromPid, toPid)

        put' (SEND toPid message) = do
            putTerm (sendTag, unused, toPid)
            putTerm message

        put' (EXIT fromPid toPid reason) = do
            putTerm (exitTag, fromPid, toPid, reason)

        put' (UNLINK fromPid toPid) = do
            putTerm (unlinkTag, fromPid, toPid)

        put' NODE_LINK = do
            putTerm (Tuple1 nodeLinkTag)

        put' (REG_SEND fromPid toName message) = do
            putTerm (regSendTag, fromPid, unused, toName)
            putTerm message

        put' (GROUP_LEADER fromPid toPid) = do
            putTerm (groupLeaderTag, fromPid, toPid)

        put' (EXIT2 fromPid toPid reason) = do
            putTerm (exit2Tag, fromPid, toPid, reason)
    get = do
        expectedLen <- getWord32be
        if expectedLen == 0
            then return TICK
            else do
                pos0 <- bytesRead
                matchWord8 pass_through
                controlMessage <- get'
                pos1 <- bytesRead
                let actualLen = pos1 - pos0
                if (fromIntegral expectedLen) == actualLen
                    then do
                        return controlMessage
                    else do
                        fail "Bad control message length"
      where
        badControlMsg term = fail ("Bad control message: " ++ show term)
        get' = do
            term <- getTerm
            res <- runMaybeT $ get'' term
            maybe (badControlMsg term) return res
          where
            get'' :: Term -> MaybeT Get ControlMessage
            get'' term = getLINK
                         <|> getSEND
                         <|> getEXIT
                         <|> getUNLINK
                         <|> getNODE_LINK
                         <|> getREG_SEND
                         <|> getGROUP_LEADER
                         <|> getEXIT2
              where
                getLINK = do
                    (_ :: TlinkTag, p2, p3) <- fromTermA term
                    return (LINK p2 p3)
                getSEND = do
                    (_ :: TsendTag, _ :: Term, p1) <- fromTermA term
                    message <- lift getTerm
                    return (SEND p1 message)
                getEXIT = do
                    (_ :: TexitTag, p2, p3, p4) <- fromTermA term
                    return (EXIT p2 p3 p4)
                getUNLINK = do
                    (_ :: TunlinkTag, p2, p3) <- fromTermA term
                    return (UNLINK p2 p3)
                getNODE_LINK = do
                    (_ :: Tuple1 TnodeLinkTag) <- fromTermA term
                    return NODE_LINK
                getREG_SEND = do
                    (_ :: TregSendTag, p2, _p3 :: Term, p4) <- fromTermA term
                    message <- lift getTerm
                    return (REG_SEND p2 p4 message)
                getGROUP_LEADER = do
                    (_ :: TgroupLeaderTag, p2, p3) <- fromTermA term
                    return (GROUP_LEADER p2 p3)
                getEXIT2 = do
                    (_ :: Texit2Tag, p2, p3, p4) <- fromTermA term
                    return (EXIT2 p2 p3 p4)

--------------------------------------------------------------------------------
pass_through :: Word8
pass_through = 112

type TlinkTag = SInteger 1

linkTag :: TlinkTag
linkTag = SInteger

type TsendTag = SInteger 2

sendTag :: TsendTag
sendTag = SInteger

type TexitTag = SInteger 3

exitTag :: TexitTag
exitTag = SInteger

type TunlinkTag = SInteger 4

unlinkTag :: TunlinkTag
unlinkTag = SInteger

type TnodeLinkTag = SInteger 5

nodeLinkTag :: TnodeLinkTag
nodeLinkTag = SInteger

type TregSendTag = SInteger 6

regSendTag :: TregSendTag
regSendTag = SInteger

type TgroupLeaderTag = SInteger 7

groupLeaderTag :: TgroupLeaderTag
groupLeaderTag = SInteger

type Texit2Tag = SInteger 8

exit2Tag :: Texit2Tag
exit2Tag = SInteger

unused :: Term
unused = atom ""

instance Arbitrary ControlMessage where
    arbitrary = oneof [ pure TICK
                      , LINK <$> arbitrary <*> arbitrary
                      , SEND <$> arbitrary <*> arbitrary
                      , EXIT <$> arbitrary <*> arbitrary <*> arbitrary
                      , UNLINK <$> arbitrary <*> arbitrary
                      , pure NODE_LINK
                      , REG_SEND <$> arbitrary <*> arbitrary <*> arbitrary
                      , GROUP_LEADER <$> arbitrary <*> arbitrary
                      , EXIT2 <$> arbitrary <*> arbitrary <*> arbitrary
                      ]
