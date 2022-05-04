{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Strict #-}

module Foreign.Erlang.Handshake
  ( HandshakeData (..),
    doConnect,
    doAccept,
    Name (..),
    Status (..),
    Challenge (..),
    ChallengeReply (..),
    ChallengeAck (..),
    otp23Name,
    otp23DistributionFlags,
  )
where

import Control.Monad (unless, when)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.Ix (inRange)
import Data.Text (Text)
import Foreign.Erlang.Digest
import Foreign.Erlang.NodeData
import UnliftIO
import Util.Binary
import qualified Data.Text.Encoding as T

data HandshakeData = HandshakeData
  { name :: Name,
    nodeData :: NodeData,
    cookie :: Text
  }

nodeTypeR6, challengeStatus, challengeReply, challengeAck :: Char
nodeTypeR6 = 'n'
challengeStatus = 's'
challengeReply = 'r'
challengeAck = 'a'

data Name = Name
  { n_distVer :: DistributionVersion,
    n_distFlags :: DistributionFlags,
    n_nodeName :: Text
  }
  deriving (Eq, Show)

-- | An Erlang OTP-23 compatible 'Name' containing the
--   'otp23DistributionFlags'.
otp23Name :: Text -> Name
otp23Name n =
  Name
    { n_distVer = R6B,
      n_distFlags = otp23DistributionFlags,
      n_nodeName = n
    }

-- | The distribution flags mandatory for OTP-23.
-- This contains 'BIG_CREATION' and 'ATOM_UTF8',
-- so the 'Term's received using distributed Erlang
-- contain 'NewPid's and 'AtomUtf8' terms instead of
-- 'Pid' and 'Atom' terms.
otp23DistributionFlags :: DistributionFlags
otp23DistributionFlags =
  DistributionFlags
    [ EXTENDED_REFERENCES,
      FUN_TAGS,
      NEW_FUN_TAGS,
      EXTENDED_PIDS_PORTS,
      BIT_BINARIES,
      NEW_FLOATS,
      UTF8_ATOMS,
      BIG_CREATION
    ]

instance Binary Name where
  put Name {n_distVer, n_distFlags, n_nodeName} =
    putWithLength16be $ do
      putChar8 nodeTypeR6
      put n_distVer
      put n_distFlags
      putByteString (T.encodeUtf8 n_nodeName)
  get = do
    len <- getWord16be
    (((), n_distVer, n_distFlags), l) <-
      getWithLength16be $
        (,,) <$> matchChar8 nodeTypeR6
          <*> get
          <*> get
    n_nodeName <- T.decodeUtf8 <$> getByteString (fromIntegral (len - l))
    return Name {n_distVer, n_distFlags, n_nodeName}

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

data Challenge = Challenge
  { c_distVer :: DistributionVersion,
    c_distFlags :: DistributionFlags,
    c_challenge :: Word32,
    c_nodeName :: Text
  }
  deriving (Eq, Show)

instance Binary Challenge where
  put Challenge {c_distVer, c_distFlags, c_challenge, c_nodeName} =
    putWithLength16be $ do
      putChar8 nodeTypeR6
      put c_distVer
      put c_distFlags
      putWord32be c_challenge
      putByteString (T.encodeUtf8 c_nodeName)
  get = do
    len <- getWord16be
    (((), c_distVer, c_distFlags, c_challenge), l) <-
      getWithLength16be $
        (,,,) <$> matchChar8 nodeTypeR6
          <*> get
          <*> get
          <*> getWord32be
    c_nodeName <- T.decodeUtf8 <$> getByteString (fromIntegral (len - l))
    return Challenge {c_distVer, c_distFlags, c_challenge, c_nodeName}

data ChallengeReply = ChallengeReply
  { cr_challenge :: Word32,
    cr_digest :: BS.ByteString
  }
  deriving (Eq, Show)

instance Binary ChallengeReply where
  put ChallengeReply {cr_challenge, cr_digest} =
    putWithLength16be $ do
      putChar8 challengeReply
      putWord32be cr_challenge
      putByteString cr_digest
  get = do
    len <- getWord16be
    (((), cr_challenge), l) <-
      getWithLength16be $
        (,) <$> matchChar8 challengeReply
          <*> getWord32be
    cr_digest <- getByteString (fromIntegral (len - l))
    return ChallengeReply {cr_challenge, cr_digest}

newtype ChallengeAck = ChallengeAck {ca_digest :: BS.ByteString}
  deriving (Eq, Show)

instance Binary ChallengeAck where
  put ChallengeAck {ca_digest} =
    putWithLength16be $ do
      putChar8 challengeAck
      putByteString ca_digest
  get = do
    len <- getWord16be
    ((), l) <- getWithLength16be $ matchChar8 challengeAck
    ca_digest <- getByteString (fromIntegral (len - l))
    return ChallengeAck {ca_digest}

doConnect ::
  (MonadUnliftIO m, MonadIO m) =>
  (forall o. Binary o => o -> m ()) ->
  (forall i. (Binary i) => m i) ->
  HandshakeData ->
  m ()
doConnect send recv HandshakeData {name, nodeData = NodeData {loVer, hiVer}, cookie} = do
  send name
  do
    her_status <- recv
    when (her_status /= Ok) (throwIO (BadHandshakeStatus her_status))

  Challenge {c_distVer = her_distVer, c_challenge = her_challenge} <- recv
  checkVersionRange her_distVer loVer hiVer

  our_challenge <- liftIO genChallenge
  send
    ChallengeReply
      { cr_challenge = our_challenge,
        cr_digest = genDigest her_challenge cookie
      }
  ChallengeAck {ca_digest = her_digest} <- recv
  checkCookie her_digest our_challenge cookie

newtype BadHandshakeStatus = BadHandshakeStatus Status
  deriving (Show)

instance Exception BadHandshakeStatus

doAccept ::
  (MonadUnliftIO m) =>
  (forall o. Binary o => o -> m ()) -> -- TODO
  (forall i. (Binary i) => m i) ->
  HandshakeData ->
  m Text
doAccept send recv HandshakeData {name = Name {n_distFlags, n_nodeName}, nodeData = NodeData {loVer, hiVer}, cookie} = do
  Name {n_distVer = her_distVer, n_nodeName = her_nodeName} <- recv
  checkVersionRange her_distVer loVer hiVer

  send Ok

  our_challenge <- liftIO genChallenge
  send
    Challenge
      { c_distVer = R6B,
        c_distFlags = n_distFlags,
        c_challenge = our_challenge,
        c_nodeName = n_nodeName
      }

  ChallengeReply {cr_challenge = her_challenge, cr_digest = her_digest} <- recv
  checkCookie her_digest our_challenge cookie

  send ChallengeAck {ca_digest = genDigest her_challenge cookie}
  return her_nodeName

checkVersionRange ::
  MonadUnliftIO m =>
  DistributionVersion ->
  DistributionVersion ->
  DistributionVersion ->
  m ()
checkVersionRange herVersion lowVersion highVersion =
  unless
    (inRange (lowVersion, highVersion) herVersion)
    ( throwIO
        DistributionVersionMismatch
          { herVersion,
            lowVersion,
            highVersion
          }
    )

checkCookie :: MonadUnliftIO m => BS.ByteString -> Word32 -> Text -> m ()
checkCookie her_digest our_challenge cookie =
  unless
    (her_digest == genDigest our_challenge cookie)
    (throwIO CookieMismatch)

data DistributionVersionMismatch = DistributionVersionMismatch
  { herVersion :: DistributionVersion,
    lowVersion :: DistributionVersion,
    highVersion :: DistributionVersion
  }
  deriving (Show)

instance Exception DistributionVersionMismatch

data CookieMismatch = CookieMismatch
  deriving (Show)

instance Exception CookieMismatch
