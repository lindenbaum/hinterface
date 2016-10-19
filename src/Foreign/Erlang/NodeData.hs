{-# LANGUAGE Strict #-}
module Foreign.Erlang.NodeData
    ( DistributionVersion(..)
    , matchDistributionVersion
    , DistributionFlag(..)
    , DistributionFlags(..)
    , NodeType(..)
    , NodeProtocol(..)
    , NodeData(..)
    ) where

import qualified Data.ByteString as BS
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get
import           Data.Bits

import           Util.Binary

--------------------------------------------------------------------------------
data DistributionVersion = Zero | R4 | NeverUsed | R5C | R6 | R6B
    deriving (Eq, Show, Enum, Bounded, Ord)

instance Binary DistributionVersion where
    put = putWord16be . fromIntegral . fromEnum
    get = do
        c <- getWord16be
        return $ toEnum $ fromIntegral c

--------------------------------------------------------------------------------
matchDistributionVersion :: NodeData -> NodeData -> Maybe DistributionVersion
matchDistributionVersion NodeData{protocol = localProto,hiVer = localHi,loVer = localLo} NodeData{protocol = remoteProto,hiVer = remoteHi,loVer = remoteLo}
    | localProto /= remoteProto =
          Nothing
    | localHi < remoteLo = Nothing
    | localLo > remoteHi = Nothing
    | otherwise = Just (max localHi remoteHi)

--------------------------------------------------------------------------------
data DistributionFlag = PUBLISHED            --  The node should be published and part of the global namespace
                      | ATOM_CACHE           --  The node implements an atom cache (obsolete)
                      | EXTENDED_REFERENCES  --  The node implements extended (3 * 32 bits) references. This is required today. If not present connection will be refused.
                      | DIST_MONITOR         --  The node implements distributed process monitoring.
                      | FUN_TAGS             --  The node uses separate tag for fun's (lambdas) in the distribution protocol.
                      | DIST_MONITOR_NAME    --  The node implements distributed named process monitoring.
                      | HIDDEN_ATOM_CACHE    --  The (hidden) node implements atom cache (obsolete)
                      | NEW_FUN_TAGS         --  The node understand new fun-tags
                      | EXTENDED_PIDS_PORTS  --  The node is capable of handling extended pids and ports. This is required today. If not present connection will be refused.
                      | EXPORT_PTR_TAG
                      | BIT_BINARIES
                      | NEW_FLOATS           --  The node understands new float format
                      | UNICODE_IO
                      | DIST_HDR_ATOM_CACHE  --  The node implements atom cache in distribution header.
                      | SMALL_ATOM_TAGS      --  The node understand the SMALL_ATOM_EXT tag
                      | UTF8_ATOMS           --  The node understand UTF-8 encoded atoms
    deriving (Eq, Show, Enum, Bounded, Ord)

newtype DistributionFlags = DistributionFlags [DistributionFlag]
    deriving (Eq, Show)

instance Binary DistributionFlags where
    put (DistributionFlags flags) = do
        putWord32be $ toBits flags
      where
        toBits :: [DistributionFlag] -> Word32
        toBits = foldl (flip $ (.|.) . toBit) 0
    get = do
        (DistributionFlags . fromBits) <$> getWord32be
      where
        fromBits :: Word32 -> [DistributionFlag]
        fromBits bits = [ flag
                        | flag <- [minBound .. maxBound]
                        , bits .&. toBit flag /= 0 ]

toBit :: DistributionFlag -> Word32
toBit PUBLISHED = 0x00001
toBit ATOM_CACHE = 0x00002
toBit EXTENDED_REFERENCES =
    0x00004
toBit DIST_MONITOR = 0x00008
toBit FUN_TAGS = 0x00010
toBit DIST_MONITOR_NAME =
    0x00020 -- NOT USED
toBit HIDDEN_ATOM_CACHE =
    0x00040 -- NOT SUPPORTED
toBit NEW_FUN_TAGS = 0x00080
toBit EXTENDED_PIDS_PORTS =
    0x00100
toBit EXPORT_PTR_TAG = 0x00200 -- NOT SUPPORTED
toBit BIT_BINARIES = 0x00400
toBit NEW_FLOATS = 0x00800
toBit UNICODE_IO = 0x01000
toBit DIST_HDR_ATOM_CACHE =
    0x02000
toBit SMALL_ATOM_TAGS = 0x04000
toBit UTF8_ATOMS = 0x10000

--------------------------------------------------------------------------------
data NodeType = NormalNode | HiddenNode
    deriving (Eq, Show, Enum, Bounded)

instance Binary NodeType where
    put NormalNode = putWord8 77
    put HiddenNode = putWord8 72
    get = do
        nodeType <- getWord8
        case nodeType of
            77 -> return NormalNode
            72 -> return HiddenNode
            _ -> fail $ "Bad node type: " ++ show nodeType

--------------------------------------------------------------------------------
data NodeProtocol = TcpIpV4
    deriving (Eq, Show, Enum, Bounded)

instance Binary NodeProtocol where
    put = putWord8 . fromIntegral . fromEnum
    get = do
        c <- getWord8
        return $ toEnum $ fromIntegral c

--------------------------------------------------------------------------------
data NodeData = NodeData { portNo    :: Word16
                         , nodeType  :: NodeType
                         , protocol  :: NodeProtocol
                         , hiVer     :: DistributionVersion
                         , loVer     :: DistributionVersion
                         , aliveName :: BS.ByteString
                         , extra     :: BS.ByteString
                         }
    deriving (Eq, Show)

--------------------------------------------------------------------------------
instance Binary NodeData where
    put NodeData{portNo,nodeType,protocol,hiVer,loVer,aliveName,extra} = do
        putWord16be portNo
        put nodeType
        put protocol
        put hiVer
        put loVer
        putLength16beByteString aliveName
        putLength16beByteString extra
    get = do
        NodeData <$> getWord16be <*> get <*> get <*> get <*> get <*> getLength16beByteString <*> getLength16beByteString
