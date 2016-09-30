{-# LANGUAGE Rank2Types #-}

module Language.Erlang.Handshake
    ( connectNodes
    , acceptConnection
    , Name(..)
    , Status(..)
    , Challenge(..)
    , ChallengeReply(..)
    , ChallengeAck(..)
    ) where

import           Control.Monad              ( unless )

import qualified Data.ByteString            as BS
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Util.Socket
import           Util.BufferedSocket
import           Util.Binary
import           Util.Util

import           Data.IOx
import           Language.Erlang.Digest
import           Language.Erlang.NodeState
import           Language.Erlang.NodeData
import           Language.Erlang.Epmd
import           Language.Erlang.Term
import           Language.Erlang.Mailbox
import           Language.Erlang.Connection

--------------------------------------------------------------------------------
nodeTypeR6, challengeStatus, challengeReply, challengeAck :: Char
nodeTypeR6 = 'n'

challengeStatus = 's'

challengeReply = 'r'

challengeAck = 'a'

--------------------------------------------------------------------------------
data Name = Name { n_distVer   :: DistributionVersion
                 , n_distFlags :: DistributionFlags
                 , n_nodeName  :: BS.ByteString
                 }
    deriving (Eq, Show)

instance Binary Name where
    put Name{n_distVer,n_distFlags,n_nodeName} =
        putWithLength16be $ do
            putChar8 nodeTypeR6
            put n_distVer
            put n_distFlags
            putByteString n_nodeName
    get = do
        len <- getWord16be
        (((), n_distVer, n_distFlags), l) <- getWithLength16be $ (,,) <$> matchChar8 nodeTypeR6 <*> get <*> get
        n_nodeName <- getByteString (fromIntegral (len - l))
        return Name { n_distVer, n_distFlags, n_nodeName }

--------------------------------------------------------------------------------
data Status = Ok | OkSimultaneous | Nok | NotAllowed | Alive
    deriving (Eq, Show, Bounded, Enum)

instance Binary Status where
    put status = putWithLength16be $ do
        putChar8 challengeStatus
        case status of
            Ok -> putByteString "ok"
            OkSimultaneous -> putByteString "ok_simultaneous"
            Nok -> putByteString "nok"
            NotAllowed -> putByteString "not_allowed"
            Alive -> putByteString "alive"
    get = do
        len <- getWord16be
        ((), l) <- getWithLength16be $ matchChar8 challengeStatus
        status <- getByteString (fromIntegral (len - l))
        case status of
            "ok" -> return Ok
            "ok_simultaneous" -> return OkSimultaneous
            "nok" -> return Nok
            "not_allowed" -> return NotAllowed
            "alive" -> return Alive
            _ -> fail $ "Bad status: " ++ show status

--------------------------------------------------------------------------------
data Challenge = Challenge { c_distVer   :: DistributionVersion
                           , c_distFlags :: DistributionFlags
                           , c_challenge :: Word32
                           , c_nodeName  :: BS.ByteString
                           }
    deriving (Eq, Show)

instance Binary Challenge where
    put Challenge{c_distVer,c_distFlags,c_challenge,c_nodeName} =
        putWithLength16be $ do
            putChar8 nodeTypeR6
            put c_distVer
            put c_distFlags
            putWord32be c_challenge
            putByteString c_nodeName
    get = do
        len <- getWord16be
        (((), c_distVer, c_distFlags, c_challenge), l) <- getWithLength16be $
                                                              (,,,) <$> matchChar8 nodeTypeR6
                                                                    <*> get
                                                                    <*> get
                                                                    <*> getWord32be
        c_nodeName <- getByteString (fromIntegral (len - l))
        return Challenge { c_distVer, c_distFlags, c_challenge, c_nodeName }

--------------------------------------------------------------------------------
data ChallengeReply = ChallengeReply { cr_challenge :: Word32
                                     , cr_digest    :: BS.ByteString
                                     }
    deriving (Eq, Show)

instance Binary ChallengeReply where
    put ChallengeReply{cr_challenge,cr_digest} =
        putWithLength16be $ do
            putChar8 challengeReply
            putWord32be cr_challenge
            putByteString cr_digest
    get = do
        len <- getWord16be
        (((), cr_challenge), l) <- getWithLength16be $ (,) <$> matchChar8 challengeReply <*> getWord32be
        cr_digest <- getByteString (fromIntegral (len - l))
        return ChallengeReply { cr_challenge, cr_digest }

--------------------------------------------------------------------------------
data ChallengeAck = ChallengeAck { ca_digest :: BS.ByteString }
    deriving (Eq, Show)

instance Binary ChallengeAck where
    put ChallengeAck{ca_digest} =
        putWithLength16be $ do
            putChar8 challengeAck
            putByteString ca_digest
    get = do
        len <- getWord16be
        ((), l) <- getWithLength16be $ matchChar8 challengeAck
        ca_digest <- getByteString (fromIntegral (len - l))
        return ChallengeAck { ca_digest }

--------------------------------------------------------------------------------
connectNodes :: BS.ByteString
             -> NodeData
             -> DistributionFlags
             -> Term
             -> BS.ByteString
             -> NodeState Term Term Mailbox Connection
             -> IOx Connection
connectNodes localName localNode localFlags remoteName cookie nodeState = do
    let (remoteAlive, remoteHost) =
            splitNodeName remoteName

    remoteNode@NodeData{portNo = remotePort} <- lookupNode remoteAlive remoteHost
    sock <- connectSocket remoteHost remotePort >>= makeBuffered

    localVersion <- maybeErrorX illegalOperationErrorType
                                "version mismatch"
                                (matchDistributionVersion localNode remoteNode)

    doConnect (runPutSocket2 sock) (runGetSocket2 sock) (Name localVersion localFlags localName) cookie

    newConnection sock nodeState remoteName

--------------------------------------------------------------------------------
acceptConnection :: Socket
                 -> BS.ByteString
                 -> NodeData
                 -> DistributionFlags
                 -> NodeState Term Term Mailbox Connection
                 -> BS.ByteString
                 -> IOx Connection
acceptConnection sock localName localNode localFlags nodeState cookie = do
    sock' <- acceptSocket sock >>= makeBuffered
    remoteName <- doAccept (runPutSocket2 sock') (runGetSocket2 sock') localName localNode localFlags cookie
    newConnection sock' nodeState remoteName

--------------------------------------------------------------------------------
doConnect :: (forall o. Binary o => o -> IOx ()) -> (forall i. (Binary i) => IOx i) -> Name -> BS.ByteString -> IOx ()
doConnect send recv name@Name{n_distVer = our_distVer} cookie = do
    send name

    her_status <- recv
    case her_status of
        Ok -> return ()
        _ -> errorX userErrorType $ "Bad status: " ++ show her_status

    Challenge{c_distVer = her_distVer,c_distFlags = her_distFlags,c_challenge = her_challenge,c_nodeName = her_nodeName} <- recv
    unless (our_distVer == her_distVer) (errorX userErrorType "Version mismatch")

    our_challenge <- genChallenge
    let our_digest = genDigest her_challenge cookie
    send ChallengeReply { cr_challenge = our_challenge, cr_digest = our_digest }

    ChallengeAck{ca_digest = her_digest} <- recv
    checkCookie her_digest our_challenge cookie

--------------------------------------------------------------------------------
doAccept :: (forall o. Binary o => o -> IOx ())
         -> (forall i. (Binary i) => IOx i)
         -> BS.ByteString
         -> NodeData
         -> DistributionFlags
         -> BS.ByteString
         -> IOx Term
doAccept send recv localNodeName NodeData{loVer,hiVer} localFlags cookie = do
    Name{n_distVer = her_distVer,n_distFlags = her_distFlags,n_nodeName = her_nodeName} <- recv
    unless (loVer <= her_distVer && her_distVer <= hiVer) (errorX userErrorType "Version mismatch")

    send Ok

    our_challenge <- genChallenge
    send Challenge { c_distVer = R6B
                   , c_distFlags = localFlags
                   , c_challenge = our_challenge
                   , c_nodeName = localNodeName
                   }

    ChallengeReply{cr_challenge = her_challenge,cr_digest = her_digest} <- recv
    checkCookie her_digest our_challenge cookie

    let our_digest = genDigest her_challenge cookie
    send ChallengeAck { ca_digest = our_digest }
    return (atom her_nodeName)

checkCookie :: BS.ByteString -> Word32 -> BS.ByteString -> IOx ()
checkCookie her_digest our_challenge cookie =
    unless (her_digest == genDigest our_challenge cookie) (errorX userErrorType "Cookie mismatch")
