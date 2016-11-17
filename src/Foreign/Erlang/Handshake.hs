{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Strict     #-}

module Foreign.Erlang.Handshake
    ( HandshakeData(..)
    , doConnect
    , doAccept
    , Name(..)
    , Status(..)
    , Challenge(..)
    , ChallengeReply(..)
    , ChallengeAck(..)
    ) where

import           Control.Monad           ( unless, when )
import           Util.IOExtra
import           Data.Ix                 ( inRange )

import qualified Data.ByteString         as BS
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Util.Binary
import           Foreign.Erlang.Digest
import           Foreign.Erlang.NodeData

data HandshakeData = HandshakeData { name     :: Name
                                   , nodeData :: NodeData
                                   , cookie   :: BS.ByteString
                                   }

nodeTypeR6, challengeStatus, challengeReply, challengeAck :: Char
nodeTypeR6 = 'n'

challengeStatus = 's'

challengeReply = 'r'

challengeAck = 'a'

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
        (((), n_distVer, n_distFlags), l) <- getWithLength16be $
                                                 (,,) <$> matchChar8 nodeTypeR6
                                                      <*> get
                                                      <*> get
        n_nodeName <- getByteString (fromIntegral (len - l))
        return Name { n_distVer, n_distFlags, n_nodeName }

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
        (((), cr_challenge), l) <- getWithLength16be $
                                       (,) <$> matchChar8 challengeReply
                                           <*> getWord32be
        cr_digest <- getByteString (fromIntegral (len - l))
        return ChallengeReply { cr_challenge, cr_digest }

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

doConnect :: (MonadCatch m, MonadIO m)
          => (forall o. Binary o => o -> m ())
          -> (forall i. (Binary i) => m i)
          -> HandshakeData
          -> m ()
doConnect send recv HandshakeData{name,nodeData = NodeData{loVer,hiVer},cookie} = do
    send name
    do
        her_status <- recv
        when (her_status /= Ok) (throwM (BadHandshakeStatus her_status))

    Challenge{c_distVer = her_distVer,c_challenge = her_challenge} <- recv
    checkVersionRange her_distVer loVer hiVer

    our_challenge <- liftIO genChallenge
    send ChallengeReply { cr_challenge = our_challenge
                        , cr_digest = genDigest her_challenge cookie
                        }
    ChallengeAck{ca_digest = her_digest} <- recv
    checkCookie her_digest our_challenge cookie

newtype BadHandshakeStatus = BadHandshakeStatus Status
    deriving Show

instance Exception BadHandshakeStatus

doAccept :: (MonadCatch m, MonadIO m)
         => (forall o. Binary o => o -> m ()) -- TODO
         -> (forall i. (Binary i) => m i)
         -> HandshakeData
         -> m BS.ByteString
doAccept send recv HandshakeData{name = Name{n_distFlags,n_nodeName},nodeData = NodeData{loVer,hiVer},cookie} = do
    Name{n_distVer = her_distVer,n_nodeName = her_nodeName} <- recv
    checkVersionRange her_distVer loVer hiVer

    send Ok

    our_challenge <- liftIO genChallenge
    send Challenge { c_distVer = R6B
                   , c_distFlags = n_distFlags
                   , c_challenge = our_challenge
                   , c_nodeName = n_nodeName
                   }

    ChallengeReply{cr_challenge = her_challenge,cr_digest = her_digest} <- recv
    checkCookie her_digest our_challenge cookie

    send ChallengeAck { ca_digest = genDigest her_challenge cookie }
    return her_nodeName

checkVersionRange :: MonadThrow m
                  => DistributionVersion
                  -> DistributionVersion
                  -> DistributionVersion
                  -> m ()
checkVersionRange herVersion lowVersion highVersion =
    unless (inRange (lowVersion, highVersion) herVersion)
           (throwM DistributionVersionMismatch { herVersion
                                               , lowVersion
                                               , highVersion
                                               })

checkCookie :: MonadThrow m => BS.ByteString -> Word32 -> BS.ByteString -> m ()
checkCookie her_digest our_challenge cookie =
    unless (her_digest == genDigest our_challenge cookie)
           (throwM CookieMismatch)

data DistributionVersionMismatch =
      DistributionVersionMismatch { herVersion  :: DistributionVersion
                                  , lowVersion  :: DistributionVersion
                                  , highVersion :: DistributionVersion
                                  }
    deriving Show

instance Exception DistributionVersionMismatch

data CookieMismatch = CookieMismatch
    deriving Show

instance Exception CookieMismatch
