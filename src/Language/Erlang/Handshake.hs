{-# LANGUAGE PackageImports #-}

module Language.Erlang.Handshake
    ( connectNodes
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

import           Util.IOx
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
        (((), version, flags), l) <- getWithLength16be $ (,,) <$> matchChar8 nodeTypeR6 <*> get <*> get
        name <- getByteString (fromIntegral (len - l))
        return $ Name version flags name

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
        (((), version, flags, challenge), l) <- getWithLength16be $
                                                    (,,,) <$> matchChar8 nodeTypeR6 <*> get <*> get <*> getWord32be
        name <- getByteString (fromIntegral (len - l))
        return $ Challenge version flags challenge name

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
        (((), challenge), l) <- getWithLength16be $ (,) <$> matchChar8 challengeReply <*> getWord32be
        digest <- getByteString (fromIntegral (len - l))
        return $ ChallengeReply challenge digest

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
        digest <- getByteString (fromIntegral (len - l))
        return $ ChallengeAck digest

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

    Challenge{c_distFlags,c_nodeName} <- handshake sock (Name localVersion localFlags localName) cookie
    unless (c_nodeName == remoteAlive `BS.append` "@" `BS.append` remoteHost) $
        errorX userErrorType "Remote node name mismatch"

    newConnection sock nodeState remoteName

handshake :: BufferedSocket -> Name -> BS.ByteString -> IOx Challenge
handshake sock n cookie = do
    send n
    s <- recv
    case s of
        Ok -> return ()
        _ -> errorX userErrorType $ "Bad status: " ++ show s
    c <- recv
    checkVersion n c
    r <- reply c
    send r
    a <- recv
    checkCookie r a
    return c
  where
    send :: (Binary a) => a -> IOx ()
    send = runPutSocket2 sock

    recv :: (Binary a) => IOx a
    recv = runGetSocket2 sock

    checkVersion :: Name -> Challenge -> IOx ()
    checkVersion Name{n_distVer} Challenge{c_distVer} = do
        unless (n_distVer == c_distVer) $
            errorX userErrorType "Version mismatch"

    reply :: Challenge -> IOx ChallengeReply
    reply Challenge{c_challenge} = do
        localChallenge <- genChallenge
        return $ ChallengeReply localChallenge (genDigest c_challenge cookie)

    checkCookie :: ChallengeReply -> ChallengeAck -> IOx ()
    checkCookie ChallengeReply{cr_challenge} ChallengeAck{ca_digest} = do
        unless (ca_digest == genDigest cr_challenge cookie) $
            errorX userErrorType "Cookie mismatch"
