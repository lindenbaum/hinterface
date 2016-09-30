module Language.Erlang.Term
    ( -- * External Term Format
      Term()
    , putTerm
    , getTerm
      -- ** Conversion to and from External Term Format
    , ToTerm(..)
    , FromTerm(..)
      -- ** Constructors
    , integer
    , float
    , atom
    , port
    , pid
    , tuple
    , string
    , list
    , improperList
    , ref
      -- ** Recognizers
    , is_integer
    , is_float
    , is_atom
    , is_reference
    , is_port
    , is_pid
    , is_tuple
    , is_map
    , is_list
    , is_binary
      -- ** Accessors
    , node
    , atom_name
    , length
    , element
    , to_string
    , to_integer
      -- ** Matchers
    , match_atom
    , match_tuple
      -- ** Utilities
    , splitNodeName
    ) where

import           Prelude               hiding ( id, length )
import qualified Prelude               as P ( id )

import           Control.Category      ( (>>>) )
import           Control.Monad         as M ( replicateM )

import           Data.String
import           Data.ByteString       ( ByteString )
import           Data.ByteString.Char8 ( unpack )
import qualified Data.ByteString       as BS ( head, length, split, tail, unpack )
import qualified Data.ByteString.Char8 as CS ( pack )
import           Data.Vector           ( (!), Vector, fromList, toList )
import qualified Data.Vector           as V ( length, replicateM, tail )
import qualified Data.List             as L ( length )

import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get       hiding ( getBytes )
import           Data.Char

import           Util.Binary

--------------------------------------------------------------------------------
data Term = Integer Integer
          | Float Double
          | Atom ByteString
          | Reference ByteString Word32 Word8
          | Port ByteString Word32 Word8
          | Pid ByteString Word32 Word32 Word8
          | Tuple (Vector Term)
          | Map (Vector MapEntry)
          | Nil
          | String ByteString
          | List (Vector Term) Term
          | Binary ByteString
          | NewReference ByteString Word8 [Word32]
    deriving (Eq)

data MapEntry = MapEntry { key   :: Term
                         , value :: Term
                         }
    deriving (Eq)

-- number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
instance Ord Term where
    (Integer i) `compare` (Integer i') =
        i `compare` i'
    (Integer i) `compare` (Float d') =
        (fromIntegral i) `compare` d'
    (Integer _) `compare` _ =
        LT

    (Float d) `compare` (Float d') =
        d `compare` d'
    (Float d) `compare` (Integer i') =
        d `compare` (fromIntegral i')
    (Float _) `compare` _ = LT

    (Atom a) `compare` (Atom a') =
        a `compare` a'
    (Atom _) `compare` _ = LT

    (Reference node id creation) `compare` (Reference node' id' creation') =
        (node, id, creation) `compare` (node', id', creation')
    (Reference _ _ _) `compare` _ =
        LT

    (NewReference node creation ids) `compare` (NewReference node' creation' ids') =
        (node, creation, ids) `compare` (node', creation', ids')
    (NewReference _ _ _) `compare` _ =
        LT

    (Port node id creation) `compare` (Port node' id' creation') =
        (node, id, creation) `compare` (node', id', creation')
    (Port _ _ _) `compare` _ =
        LT

    (Pid node id serial creation) `compare` (Pid node' id' serial' creation') =
        (node, id, serial, creation) `compare` (node', id', serial', creation')
    (Pid _ _ _ _) `compare` _ =
        LT

    (Tuple v) `compare` (Tuple v') =
        v `compare` v'
    (Tuple _) `compare` _ = LT

    (Map e) `compare` (Map e') =
        e `compare` e'
    (Map _) `compare` _ = LT

    Nil `compare` Nil = EQ
    Nil `compare` _ = LT

    (String s) `compare` (String s') =
        s `compare` s'
    (String s) `compare` (List v' t') =
        (toVector s, Nil) `compare` (v', t')
    (String _) `compare` _ =
        LT

    (List v t) `compare` (List v' t') =
        (v, t) `compare` (v', t')
    (List v t) `compare` (String s') =
        (v, t) `compare` (toVector s', Nil)
    (List _ _) `compare` _ =
        LT

    (Binary b) `compare` (Binary b') =
        b `compare` b'
    (Binary _) `compare` _ =
        LT

toVector :: ByteString -> Vector Term
toVector = BS.unpack >>> map (fromIntegral >>> Integer) >>> fromList

instance Ord MapEntry where
    MapEntry{key = k,value = v} `compare` MapEntry{key = k',value = v'} =
        (k, v) `compare` (k', v') -- FIXME integer keys are less than float keys

instance Show Term where
    show (Integer i) = show i
    show (Float d) = show d
    show (Atom a) = unpack a
    show (Reference nodeName id _creation) =
        "#Ref<" ++ unpack nodeName ++ "." ++ show id ++ ">"
    show (Port nodeName id _creation) =
        "#Port<" ++ unpack nodeName ++ "." ++ show id ++ ">"
    show (Pid nodeName id serial _creation) =
        "#Pid<" ++ unpack nodeName ++ "." ++ show id ++ "." ++ show serial ++ ">"
    show (Tuple v) = "{" ++ showVectorAsList v ++ "}"
    show (Map e) = "#{" ++ showVectorAsList e ++ "}"
    show Nil = "[]"
    show (String s) = show s
    show (List v Nil) = "[" ++ showVectorAsList v ++ "]"
    show (List v t) = "[" ++ showVectorAsList v ++ "|" ++ show t ++ "]"
    show (Binary b) = "<<" ++ showByteStringAsIntList b ++ ">>"
    show (NewReference nodeName _creation ids) =
        "#Ref<" ++ unpack nodeName ++ concat (map (\id -> "." ++ show id) ids) ++ ">"

instance Show MapEntry where
    show MapEntry{key,value} =
        show key ++ " => " ++ show value

showVectorAsList :: Show a => (Vector a) -> String
showVectorAsList v
    | V.length v == 0 = ""
    | V.length v == 1 = show (v ! 0)
    | otherwise = show (v ! 0) ++ concat (map (\t -> "," ++ show t) $ toList $ V.tail v)

showByteStringAsIntList :: ByteString -> String
showByteStringAsIntList b
    | BS.length b == 0 = ""
    | BS.length b == 1 = show (BS.head b)
    | otherwise = show (BS.head b) ++ concat (map (\t -> "," ++ show t) $ BS.unpack $ BS.tail b)

instance IsString Term where
    fromString = atom . CS.pack

--------------------------------------------------------------------------------
class ToTerm a where
    toTerm :: a -> Term

class FromTerm a where
    fromTerm :: Term -> Maybe a

--------------------------------------------------------------------------------
-- | Construct an integer
integer :: Integer -- ^ Int
        -> Term
integer = Integer

-- | Construct a float
float :: Double -- ^ IEEE float
      -> Term
float = Float

-- | Construct an atom
atom :: ByteString -- ^ AtomName
     -> Term
atom = Atom

-- reference
-- | Construct a port
port :: ByteString -- ^ Node name
     -> Word32     -- ^ ID
     -> Word8      -- ^ Creation
     -> Term
port = Port

pid :: ByteString -> Word32 -> Word32 -> Word8 -> Term
pid = Pid

-- | Construct a tuple
tuple :: [Term] -- ^ Elements
      -> Term
tuple = Tuple . fromList

-- map
-- | Construct a list
string :: ByteString -- ^ Characters
       -> Term
string = String

-- | Construct a list
list :: [Term] -- ^ Elements
     -> Term
list [] = Nil
list ts = improperList ts Nil

-- | Construct an improper list (if Tail is not Nil)
improperList :: [Term] -- ^ Elements
             -> Term   -- ^ Tail
             -> Term
improperList [] _ = error "Illegal improper list"
improperList ts t = List (fromList ts) t -- FIXME could check if is string

-- binary
-- | Construct a new reference
ref :: ByteString -- ^ Node name
    -> Word8     -- ^ Creation
    -> [Word32]  -- ^ ID ...
    -> Term
ref = NewReference

--------------------------------------------------------------------------------
is_integer, is_float, is_atom, is_reference, is_port, is_pid, is_tuple, is_map, is_list, is_binary :: Term -> Bool
-- | Test if term is an integer
is_integer (Integer _) =
    True
is_integer _ = False

-- | Test if term is a float
is_float (Float _) = True
is_float _ = False

-- | Test if term is an atom
is_atom (Atom _) = True
is_atom _ = False

-- | Test if term is a reference
is_reference (Reference _ _ _) =
    True
is_reference (NewReference _ _ _) =
    True
is_reference _ = False

-- | Test if term is a port
is_port (Port _ _ _) = True
is_port _ = False

-- | Test if term is a pid
is_pid (Pid _ _ _ _) = True
is_pid _ = False

-- | Test if term is a tuple
is_tuple (Tuple _) = True
is_tuple _ = False

-- | Test if term is a map
is_map (Map _) = True
is_map _ = False

-- | Test if term is a list
is_list Nil = True
is_list (String _) = True
is_list (List _ _) = True
is_list _ = False

-- | Test if term is a binary
is_binary (Binary _) = True
is_binary _ = False

--------------------------------------------------------------------------------
node :: Term -> Term
node (Reference nodeName _id _creation) =
    atom nodeName
node (Port nodeName _id _creation) =
    atom nodeName
node (Pid nodeName _id _serial _creation) =
    atom nodeName
node (NewReference nodeName _creation _ids) =
    atom nodeName
node term = error $ "Bad arg for node: " ++ show term

atom_name :: Term -> ByteString
atom_name (Atom name) = name
atom_name term = error $ "Bad arg for atom_name: " ++ show term

length :: Term -> Int
length (Tuple v) = V.length v
length (String bs) = BS.length bs
length (List v Nil) = V.length v
length term = error $ "Bad arg for length: " ++ show term

element :: Int -> Term -> Term
element n (Tuple v) = v ! (n - 1)
element _ term = error $ "Not a tuple: " ++ show term

to_string :: Term -> Maybe ByteString
to_string (String bs) = Just bs
to_string _ = Nothing

to_integer :: Term -> Maybe Integer
to_integer (Integer i) =
    Just i
to_integer _ = Nothing

match_tuple :: Term -> Maybe [Term]
match_tuple (Tuple v) = Just (toList v)
match_tuple _ = Nothing

match_atom :: Term -> ByteString -> Maybe ByteString
match_atom (Atom n) m
    | m == n = Just n
    | otherwise = Nothing
match_atom _ _ = Nothing

--------------------------------------------------------------------------------
splitNodeName :: Term -> (ByteString, ByteString)
splitNodeName t@(Atom a) =
    case BS.split (fromIntegral (ord '@')) a of
        [ alive, host ] -> (alive, host)
        _ -> error $ "Illegal node name: " ++ show t

splitNodeName t = error $ "Illegal node name: " ++ show t

--------------------------------------------------------------------------------
instance Binary Term where
    put (Integer i)
        | i >= 0x00 && i <= 0xFF = do
              putWord8 small_integer_ext
              putWord8 (fromIntegral i)
        | i >= -0x80000000 && i <= 0x7FFFFFFF = do
              putWord8 integer_ext
              putWord32be (fromIntegral i)
        | otherwise = do
              error $ "Integer out ouf range: " ++ show i

    put (Float d) = do
        putWord8 new_float_ext
        putDoublebe d

    put (Atom n) = do
        putAtom n

    put (Reference nodeName id creation) = do
        putWord8 reference_ext
        putAtom nodeName
        putWord32be id
        putWord8 creation

    put (Port nodeName id creation) = do
        putWord8 port_ext
        putAtom nodeName
        putWord32be id
        putWord8 creation

    put (Pid nodeName id serial creation) = do
        putWord8 pid_ext
        putAtom nodeName
        putWord32be id
        putWord32be serial
        putWord8 creation

    put (Tuple v)
        | (V.length v) < 256 = do
              putWord8 small_tuple_ext
              putWord8 $ fromIntegral (V.length v)
              mapM_ put v
        | otherwise = do
              putWord8 large_tuple_ext
              putWord32be $ fromIntegral (V.length v)
              mapM_ put v

    put (Map e) = do
        putWord8 map_ext
        putWord32be $ fromIntegral (V.length e)
        mapM_ put e

    put Nil = do
        putWord8 nil_ext

    put (String s) = do
        putWord8 string_ext
        putLength16beByteString s

    put (List v t) = do
        putWord8 list_ext
        putWord32be $ fromIntegral (V.length v)
        mapM_ put v
        put t

    put (Binary b) = do
        putWord8 binary_ext
        putLength16beByteString b

    put (NewReference node creation ids) = do
        putWord8 new_reference_ext
        putWord16be $ fromIntegral (L.length ids)
        putAtom node
        putWord8 creation
        mapM_ putWord32be ids
    get = do
        lookAhead getWord8 >>= get'
      where
        get' :: Word8 -> Get Term
        get' tag
            | tag == small_integer_ext =
                  getSmallInteger (Integer . fromIntegral)
            | tag == integer_ext =
                  getInteger (Integer . fromIntegral)
            | tag == atom_ext = getAtom Atom
            | tag == port_ext = getPort Port
            | tag == pid_ext = getPid Pid
            | tag == small_tuple_ext =
                  getSmallTuple Tuple
            | tag == large_tuple_ext =
                  getLargeTuple Tuple
            | tag == map_ext = getMap Map
            | tag == nil_ext = getNil (const Nil)
            | tag == string_ext = getString String
            | tag == list_ext = getList List
            | tag == binary_ext = getBinary Binary
            | tag == new_reference_ext =
                  getNewReference NewReference
            | tag == small_atom_ext = getSmallAtom Atom
            | tag == new_float_ext = getNewFloat Float
            | otherwise = fail $ "Unsupported tag: " ++ show tag

instance Binary MapEntry where
    put MapEntry{key,value} = do
        put key
        put value
    get = do
        MapEntry <$> get <*> get

--------------------------------------------------------------------------------
putTerm :: Term -> Put
putTerm term = do
    putWord8 magicVersion
    put term

putAtom :: ByteString -> Put
putAtom a = do
    putWord8 atom_ext
    putLength16beByteString a

--------------------------------------------------------------------------------
getTerm :: Get Term
getTerm = do
    matchWord8 magicVersion
    get

getSmallInteger :: (Word8 -> a) -> Get a
getSmallInteger f = do
    matchWord8 small_integer_ext
    f <$> getWord8

getInteger :: (Word32 -> a) -> Get a
getInteger f = do
    matchWord8 integer_ext
    f <$> getWord32be

getAtom :: (ByteString -> a) -> Get a
getAtom f = do
    matchWord8 atom_ext
    f <$> getLength16beByteString

getPort :: (ByteString -> Word32 -> Word8 -> a) -> Get a
getPort f = do
    matchWord8 port_ext
    f <$> getAtom P.id <*> getWord32be <*> getWord8

getPid :: (ByteString -> Word32 -> Word32 -> Word8 -> a) -> Get a
getPid f = do
    matchWord8 pid_ext
    f <$> getAtom P.id <*> getWord32be <*> getWord32be <*> getWord8

getSmallTuple :: (Vector Term -> a) -> Get a
getSmallTuple f = do
    matchWord8 small_tuple_ext
    f <$> (getWord8 >>= _getVector . fromIntegral)

getLargeTuple :: (Vector Term -> a) -> Get a
getLargeTuple f = do
    matchWord8 large_tuple_ext
    f <$> (getWord32be >>= _getVector . fromIntegral)

getMap :: (Vector MapEntry -> a) -> Get a
getMap f = do
    matchWord8 map_ext
    f <$> (getWord32be >>= _getVector . fromIntegral)

getNil :: (() -> a) -> Get a
getNil f = do
    f <$> matchWord8 nil_ext

getString :: (ByteString -> a) -> Get a
getString f = do
    matchWord8 string_ext
    f <$> getLength16beByteString

getList :: (Vector Term -> Term -> a) -> Get a
getList f = do
    matchWord8 list_ext
    f <$> (getWord32be >>= _getVector . fromIntegral) <*> get

getBinary :: (ByteString -> a) -> Get a
getBinary f = do
    matchWord8 binary_ext
    f <$> getLength32beByteString

getNewReference :: (ByteString -> Word8 -> [Word32] -> a) -> Get a
getNewReference f = do
    matchWord8 new_reference_ext
    len <- getWord16be
    f <$> getAtom P.id <*> getWord8 <*> _getList (fromIntegral len)

getSmallAtom :: (ByteString -> a) -> Get a
getSmallAtom f = do
    matchWord8 small_atom_ext
    f <$> getLength8ByteString

getNewFloat :: (Double -> a) -> Get a
getNewFloat f = do
    matchWord8 new_float_ext
    f <$> getDoublebe

--------------------------------------------------------------------------------
_getVector :: Binary a => Int -> Get (Vector a)
_getVector len = V.replicateM len get

_getList :: Binary a => Int -> Get [a]
_getList len = M.replicateM len get

--------------------------------------------------------------------------------
magicVersion :: Word8
magicVersion = 131

small_integer_ext, integer_ext, float_ext, atom_ext, reference_ext, port_ext, pid_ext :: Word8
small_tuple_ext, large_tuple_ext, map_ext, nil_ext, string_ext, list_ext, binary_ext :: Word8
small_big_ext, large_big_ext, new_reference_ext, small_atom_ext, fun_ext, new_fun_ext :: Word8
export_ext, bit_binary_ext, new_float_ext, atom_utf8_ext, small_atom_utf8_ext :: Word8
small_integer_ext = 97

integer_ext = 98

float_ext = 99

atom_ext = 100

reference_ext = 101

port_ext = 102

pid_ext = 103

small_tuple_ext = 104

large_tuple_ext = 105

map_ext = 116

nil_ext = 106

string_ext = 107

list_ext = 108

binary_ext = 109

small_big_ext = 110

large_big_ext = 111

new_reference_ext = 114

small_atom_ext = 115

fun_ext = 117

new_fun_ext = 112

export_ext = 113

bit_binary_ext = 77

new_float_ext = 70

atom_utf8_ext = 118

small_atom_utf8_ext = 119
