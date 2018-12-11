{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Foreign.Erlang.Term
    ( -- * External Term Format
      Term(..
          , Tuple2
          , Tuple3
          , Tuple4
          , Tuple5
          , Tuple6
          , Tuple7
          , List1
          , List2
          , List3
          , List4
          , List5
          , List6
          , List7
          , Map1
          , Map2
          , Map3
          , Map4
          , Map5
          , Map6
          , Map7
          )
    , putTerm
    , getTerm
    , MapEntry(.., (:=>))
      -- ** Conversion to and from External Term Format
    , ToTerm(..)
    , FromTerm(..)
    , fromTermA
      -- ** Constructors
    , integer
      -- *** Static numbers
    , SInteger(..)
    , float
    , atom
      -- *** Static atoms
    , SAtom(..)
    , port
    , pid
    , Pid(..)
    , tuple
    , Tuple1(..)
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
    )
where

import           GHC.TypeLits
import           Prelude                 hiding ( id
                                                , length
                                                )
import qualified Prelude                       as P
                                                ( id )
import           Control.Applicative            ( Alternative(..) )
import           Control.Category               ( (>>>) )
import           Control.DeepSeq
import           Control.Monad                 as M
                                                ( replicateM )
import           Data.String
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Char8          ( unpack )
import qualified Data.ByteString               as BS
                                                ( head
                                                , length
                                                , tail
                                                , unpack
                                                , foldr'
                                                )
import qualified Data.ByteString.Char8         as CS
                                                ( ByteString
                                                , pack
                                                , unpack
                                                )
import           Data.Vector                    ( (!)
                                                , Vector
                                                , fromList
                                                , toList
                                                )
import qualified Data.Vector                   as V
                                                ( length
                                                , replicateM
                                                , tail
                                                )
import qualified Data.List                     as L
                                                ( length
                                                , unfoldr
                                                , length
                                                )
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get         hiding ( getBytes )
import           Util.Binary
import           Test.QuickCheck
import           Data.Int
import           Data.Bits                      ( shiftR
                                                , (.&.)
                                                )
import           Data.Monoid
import           GHC.Generics

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
    deriving (Eq, Generic)

instance NFData Term

-- ** Pattern Synonyms for 'Term's
pattern Tuple2 :: Term -> Term -> Term
pattern Tuple2 t1 t2 <- Tuple (toList -> [t1,t2]) where
    Tuple2 t1 t2  = Tuple (fromList [t1,t2])

pattern Tuple3 :: Term -> Term -> Term -> Term
pattern Tuple3 t1 t2 t3 <- Tuple (toList -> [t1,t2,t3]) where
    Tuple3 t1 t2 t3  = Tuple (fromList [t1,t2,t3])

pattern Tuple4 :: Term -> Term -> Term -> Term -> Term
pattern Tuple4 t1 t2 t3 t4 <- Tuple (toList -> [t1,t2,t3,t4]) where
    Tuple4 t1 t2 t3 t4  = Tuple (fromList [t1,t2,t3,t4])

pattern Tuple5 :: Term -> Term -> Term -> Term -> Term -> Term
pattern Tuple5 t1 t2 t3 t4 t5 <- Tuple (toList -> [t1,t2,t3,t4,t5]) where
    Tuple5 t1 t2 t3 t4 t5  = Tuple (fromList [t1,t2,t3,t4,t5])

pattern Tuple6 :: Term
                        -> Term -> Term -> Term -> Term -> Term -> Term
pattern Tuple6 t1 t2 t3 t4 t5 t6 <- Tuple (toList -> [t1,t2,t3,t4,t5,t6]) where
    Tuple6 t1 t2 t3 t4 t5 t6  = Tuple (fromList [t1,t2,t3,t4,t5,t6])

pattern Tuple7 :: Term
                        -> Term -> Term -> Term -> Term -> Term -> Term -> Term
pattern Tuple7 t1 t2 t3 t4 t5 t6 t7 <- Tuple (toList -> [t1,t2,t3,t4,t5,t6,t7]) where
    Tuple7 t1 t2 t3 t4 t5 t6 t7  = Tuple (fromList [t1,t2,t3,t4,t5,t6,t7])

pattern List1 :: Term -> Term
pattern List1 t1 <- List (toList -> [t1]) Nil where
    List1 t1  = List (fromList [t1]) Nil

pattern List2 :: Term -> Term -> Term
pattern List2 t1 t2 <- List (toList -> [t1,t2]) Nil where
    List2 t1 t2  = List (fromList [t1,t2]) Nil

pattern List3 :: Term -> Term -> Term -> Term
pattern List3 t1 t2 t3 <- List (toList -> [t1,t2,t3]) Nil where
    List3 t1 t2 t3  = List (fromList [t1,t2,t3]) Nil

pattern List4 :: Term -> Term -> Term -> Term -> Term
pattern List4 t1 t2 t3 t4 <- List (toList -> [t1,t2,t3,t4]) Nil where
    List4 t1 t2 t3 t4  = List (fromList [t1,t2,t3,t4]) Nil

pattern List5 :: Term -> Term -> Term -> Term -> Term -> Term
pattern List5 t1 t2 t3 t4 t5 <- List (toList -> [t1,t2,t3,t4,t5]) Nil where
    List5 t1 t2 t3 t4 t5  = List (fromList [t1,t2,t3,t4,t5]) Nil

pattern List6 :: Term
                       -> Term -> Term -> Term -> Term -> Term -> Term
pattern List6 t1 t2 t3 t4 t5 t6 <- List (toList -> [t1,t2,t3,t4,t5,t6]) Nil where
    List6 t1 t2 t3 t4 t5 t6  = List (fromList [t1,t2,t3,t4,t5,t6]) Nil

pattern List7 :: Term
                       -> Term -> Term -> Term -> Term -> Term -> Term -> Term
pattern List7 t1 t2 t3 t4 t5 t6 t7 <- List (toList -> [t1,t2,t3,t4,t5,t6,t7]) Nil where
    List7 t1 t2 t3 t4 t5 t6 t7  = List (fromList [t1,t2,t3,t4,t5,t6,t7]) Nil

pattern (:=>) :: Term -> Term -> MapEntry
pattern k :=> v = MapEntry k v

pattern Map1 :: MapEntry -> Term
pattern Map1 t1 <- Map (toList -> [t1]) where
    Map1 t1  = Map (fromList [t1])

pattern Map2 :: MapEntry -> MapEntry -> Term
pattern Map2 t1 t2 <- Map (toList -> [t1,t2]) where
    Map2 t1 t2  = Map (fromList [t1,t2])

pattern Map3 :: MapEntry -> MapEntry -> MapEntry -> Term
pattern Map3 t1 t2 t3 <- Map (toList -> [t1,t2,t3]) where
    Map3 t1 t2 t3  = Map (fromList [t1,t2,t3])

pattern Map4 :: MapEntry
                      -> MapEntry -> MapEntry -> MapEntry -> Term
pattern Map4 t1 t2 t3 t4 <- Map (toList -> [t1,t2,t3,t4]) where
    Map4 t1 t2 t3 t4  = Map (fromList [t1,t2,t3,t4])

pattern Map5 :: MapEntry
                      -> MapEntry -> MapEntry -> MapEntry -> MapEntry -> Term
pattern Map5 t1 t2 t3 t4 t5 <- Map (toList -> [t1,t2,t3,t4,t5]) where
    Map5 t1 t2 t3 t4 t5  = Map (fromList [t1,t2,t3,t4,t5])

pattern Map6 :: MapEntry
                      -> MapEntry -> MapEntry -> MapEntry -> MapEntry -> MapEntry -> Term
pattern Map6 t1 t2 t3 t4 t5 t6 <- Map (toList -> [t1,t2,t3,t4,t5,t6]) where
    Map6 t1 t2 t3 t4 t5 t6  = Map (fromList [t1,t2,t3,t4,t5,t6])

pattern Map7 :: MapEntry
                      -> MapEntry
                      -> MapEntry
                      -> MapEntry
                      -> MapEntry
                      -> MapEntry
                      -> MapEntry
                      -> Term
pattern Map7 t1 t2 t3 t4 t5 t6 t7 <- Map (toList -> [t1,t2,t3,t4,t5,t6,t7]) where
    Map7 t1 t2 t3 t4 t5 t6 t7  = Map (fromList [t1,t2,t3,t4,t5,t6,t7])


data MapEntry = MapEntry { key   :: Term
                         , value :: Term
                         }
    deriving (Eq, Generic)

instance NFData MapEntry

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

    (Reference node' id creation) `compare` (Reference node'' id' creation') =
        (node', id, creation) `compare` (node'', id', creation')
    (Reference _ _ _) `compare` _ =
        LT

    (NewReference node' creation ids) `compare` (NewReference node'' creation' ids') =
        (node', creation, ids) `compare` (node'', creation', ids')
    (NewReference _ _ _) `compare` _ =
        LT

    (Port node' id creation) `compare` (Port node'' id' creation') =
        (node', id, creation) `compare` (node'', id', creation')
    (Port _ _ _) `compare` _ =
        LT

    (Pid node' id serial creation) `compare` (Pid node'' id' serial' creation') =
        (node', id, serial, creation) `compare` (node'', id', serial', creation')
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
    showsPrec _ (Integer i) = shows i
    showsPrec _ (Float d) = shows d
    showsPrec _ (Atom a) = showChar '\'' . showString (unpack a) . showChar '\''
    showsPrec _ (Reference nodeName id _creation) =
         showString "#Ref<" . showString (unpack nodeName) . showChar '.' . shows id . showChar '>'
    showsPrec _ (Port nodeName id _creation) =
         showString "#Port<" . showString (unpack nodeName) . showChar '.' . shows id . showChar  '>'
    showsPrec _ (Pid nodeName id serial _creation) =
         showString "#Pid<" . showString (unpack nodeName) . showChar '.' . shows id . showChar '.' . shows serial . showChar '>'
    showsPrec _ (Tuple v) = showChar '{' . showsVectorAsList v . showChar '}'
    showsPrec _ (Map e) = showString "#{" . showsVectorAsList e . showChar '}'
    showsPrec _ Nil = showString "[]"
    showsPrec _ (String s) = shows s
    showsPrec _ (List v Nil) = showChar '[' . showsVectorAsList v . showChar ']'
    showsPrec _ (List v t) = showChar '[' . showsVectorAsList v . showChar '|' . shows t . showChar ']'
    showsPrec _ (Binary b) = showString "<<" . showsByteStringAsIntList b . showString ">>"
    showsPrec _ (NewReference nodeName _creation ids) =
         showString "#Ref<" . showString (unpack nodeName) . appEndo (foldMap (\i -> Endo (showChar '.' . shows i)) ids) . showChar '>'

instance Show MapEntry where
    showsPrec _ MapEntry{key,value} =
        shows key . showString " => " . shows value

showsVectorAsList :: Show a => (Vector a) -> ShowS
showsVectorAsList v
    | V.length v == 0 = \s -> s
    | V.length v == 1 = shows (v ! 0)
    | otherwise = shows (v ! 0)
    . appEndo (foldMap (\t -> Endo (showChar ',' . shows t)) (V.tail v))

showsByteStringAsIntList :: ByteString -> ShowS
showsByteStringAsIntList b
    | BS.length b == 0
    = \s -> s
    | BS.length b == 1
    = shows (BS.head b)
    | otherwise
    = shows (BS.head b)
        . appEndo
              (foldMap (\t -> Endo (showChar ',' . shows t))
                       (BS.unpack (BS.tail b))
              )

instance IsString Term where
    fromString = atom . CS.pack

instance FromTerm Term where
    fromTerm = Just

instance ToTerm Term where
    toTerm = P.id

--------------------------------------------------------------------------------
class ToTerm a where
    toTerm :: a -> Term

class FromTerm a where
    fromTerm :: Term -> Maybe a

fromTermA :: (FromTerm a, Alternative m) => Term -> m a
fromTermA t = case fromTerm t of
    Just x  -> pure x
    Nothing -> empty

instance FromTerm () where
    fromTerm (Tuple ts) | V.length ts == 0 = Just ()
    fromTerm _ = Nothing

instance (FromTerm a) => FromTerm (Tuple1 a) where
    fromTerm (Tuple ts) | V.length ts == 1 = Tuple1 <$> fromTerm (ts ! 0)
    fromTerm _ = Nothing

instance (FromTerm a, FromTerm b) => FromTerm (a, b) where
    fromTerm (Tuple ts) | V.length ts == 2 = (,) <$> fromTerm (ts ! 0) <*> fromTerm (ts ! 1)
    fromTerm _ = Nothing

instance (FromTerm a, FromTerm b, FromTerm c) => FromTerm (a, b, c) where
    fromTerm (Tuple ts) | V.length ts == 3 = (,,) <$> fromTerm (ts ! 0) <*> fromTerm (ts ! 1) <*> fromTerm (ts ! 2)
    fromTerm _ = Nothing

instance (FromTerm a, FromTerm b, FromTerm c, FromTerm d) => FromTerm (a, b, c, d) where
    fromTerm (Tuple ts) | V.length ts == 4 = (,,,) <$> fromTerm (ts ! 0)
                                                   <*> fromTerm (ts ! 1)
                                                   <*> fromTerm (ts ! 2)
                                                   <*> fromTerm (ts ! 3)
    fromTerm _ = Nothing

instance (FromTerm a, FromTerm b, FromTerm c, FromTerm d, FromTerm e) => FromTerm (a, b, c, d, e) where
    fromTerm (Tuple ts) | V.length ts == 5 = (,,,,) <$> fromTerm (ts ! 0)
                                                    <*> fromTerm (ts ! 1)
                                                    <*> fromTerm (ts ! 2)
                                                    <*> fromTerm (ts ! 3)
                                                    <*> fromTerm (ts ! 4)
    fromTerm _ = Nothing

instance ToTerm () where
    toTerm () = tuple []

instance (ToTerm a) => ToTerm (Tuple1 a) where
    toTerm (Tuple1 a) = tuple [ toTerm a ]

instance (ToTerm a, ToTerm b) => ToTerm (a, b) where
    toTerm (a, b) = tuple [ toTerm a, toTerm b ]

instance (ToTerm a, ToTerm b, ToTerm c) => ToTerm (a, b, c) where
    toTerm (a, b, c) = tuple [ toTerm a, toTerm b, toTerm c ]

instance (ToTerm a, ToTerm b, ToTerm c, ToTerm d) => ToTerm (a, b, c, d) where
    toTerm (a, b, c, d) = tuple [ toTerm a, toTerm b, toTerm c, toTerm d ]

instance (ToTerm a, ToTerm b, ToTerm c, ToTerm d, ToTerm e) => ToTerm (a, b, c, d, e) where
    toTerm (a, b, c, d, e) =
        tuple [ toTerm a, toTerm b, toTerm c, toTerm d, toTerm e ]

instance FromTerm Integer where
    fromTerm (Integer i) = Just i
    fromTerm _ = Nothing

instance ToTerm Integer where
    toTerm = Integer

instance FromTerm String where
    fromTerm (String s) = Just (CS.unpack s)
    fromTerm _ = Nothing

instance ToTerm String where
    toTerm = String . CS.pack

--------------------------------------------------------------------------------
-- | Construct an integer
integer
    :: Integer -- ^ Int
    -> Term
integer = Integer

-- | A static/constant number.
data SInteger (n :: Nat) = SInteger

instance (KnownNat n) => Show (SInteger n) where
  showsPrec d s =
    showParen (d > 10) (showString "SInteger '" . showsPrec 11 (natVal s) . showChar '\'')

instance forall (n :: Nat) . (KnownNat n) => FromTerm (SInteger n) where
    fromTerm (Integer n') = let sn = SInteger
                                sn :: SInteger n
                            in
                                if n' == natVal sn then Just sn else Nothing
    fromTerm _ = Nothing

instance forall (n :: Nat) . (KnownNat n) => ToTerm (SInteger n) where
    toTerm = integer . natVal

-- | Construct a float
float
    :: Double -- ^ IEEE float
    -> Term
float = Float

-- | Construct an atom
atom
    :: ByteString -- ^ AtomName
    -> Term
atom = Atom

-- | A static/constant atom.
data SAtom (atom :: Symbol) = SAtom

instance (KnownSymbol atom) => Show (SAtom atom) where
  showsPrec d s =
    showParen (d > 10) (showString "SAtom '" . showString (symbolVal s) . showChar '\'')

instance forall (atom :: Symbol) . (KnownSymbol atom) => FromTerm (SAtom atom) where
    fromTerm (Atom atom') = if atom' == CS.pack (symbolVal (SAtom :: SAtom atom)) then Just SAtom else Nothing
    fromTerm _ = Nothing

instance forall (atom :: Symbol) . (KnownSymbol atom) => ToTerm (SAtom atom) where
    toTerm = atom . CS.pack . symbolVal

-- reference
-- | Construct a port
port
    :: ByteString -- ^ Node name
    -> Word32     -- ^ ID
    -> Word8      -- ^ Creation
    -> Term
port = Port

pid
    :: ByteString -- ^ Node name
    -> Word32     -- ^ ID
    -> Word32     -- ^ Serial
    -> Word8      -- ^ Creation
    -> Pid
pid = ((.) . (.) . (.) . (.)) MkPid Pid

newtype Pid = MkPid Term
    deriving (ToTerm, FromTerm, Eq, Ord)

instance Show Pid where
    show (MkPid p) = show p

-- | Construct a tuple
tuple
    :: [Term] -- ^ Elements
    -> Term
tuple = Tuple . fromList

newtype Tuple1 a = Tuple1 a
    deriving (Eq, Ord)

instance (Show a) => Show (Tuple1 a) where
  show (Tuple1 a) = "{" ++ show a ++ "}"

-- map
-- | Construct a list
string
    :: ByteString -- ^ Characters
    -> Term
string = String

-- | Construct a list
list
    :: [Term] -- ^ Elements
    -> Term
list [] = Nil
list ts = improperList ts Nil

-- | Construct an improper list (if Tail is not Nil)
improperList
    :: [Term] -- ^ Elements
    -> Term   -- ^ Tail
    -> Term
improperList [] _ = error "Illegal improper list"
improperList ts t = List (fromList ts) t -- FIXME could check if is string

-- binary
-- | Construct a new reference
ref
    :: ByteString -- ^ Node name
    -> Word8     -- ^ Creation
    -> [Word32]  -- ^ ID ...
    -> Term
ref = NewReference

--------------------------------------------------------------------------------
is_integer, is_float, is_atom, is_reference, is_port, is_pid, is_tuple, is_map, is_list, is_binary
    :: Term -> Bool
-- | Test if term is an integer
is_integer (Integer _) = True
is_integer _           = False

-- | Test if term is a float
is_float (Float _) = True
is_float _         = False

-- | Test if term is an atom
is_atom (Atom _) = True
is_atom _        = False

-- | Test if term is a reference
is_reference (Reference    _ _ _) = True
is_reference (NewReference _ _ _) = True
is_reference _                    = False

-- | Test if term is a port
is_port (Port _ _ _) = True
is_port _            = False

-- | Test if term is a pid
is_pid (Pid _ _ _ _) = True
is_pid _             = False

-- | Test if term is a tuple
is_tuple (Tuple _) = True
is_tuple _         = False

-- | Test if term is a map
is_map (Map _) = True
is_map _       = False

-- | Test if term is a list
is_list Nil        = True
is_list (String _) = True
is_list (List _ _) = True
is_list _          = False

-- | Test if term is a binary
is_binary (Binary _) = True
is_binary _          = False

--------------------------------------------------------------------------------
node :: Term -> Term
node (Reference nodeName _id _creation) = atom nodeName
node (Port nodeName _id _creation) = atom nodeName
node (Pid nodeName _id _serial _creation) = atom nodeName
node (NewReference nodeName _creation _ids) = atom nodeName
node term = error $ "Bad arg for node: " ++ show term

atom_name :: Term -> ByteString
atom_name (Atom name) = name
atom_name term        = error $ "Bad arg for atom_name: " ++ show term

length :: Term -> Int
length (Tuple  v  ) = V.length v
length (String bs ) = BS.length bs
length (List v Nil) = V.length v
length term         = error $ "Bad arg for length: " ++ show term

element :: Int -> Term -> Term
element n (Tuple v) = v ! (n - 1)
element _ term      = error $ "Not a tuple: " ++ show term

to_string :: Term -> Maybe ByteString
to_string (String bs) = Just bs
to_string _           = Nothing

to_integer :: Term -> Maybe Integer
to_integer (Integer i) = Just i
to_integer _           = Nothing

match_tuple :: Term -> Maybe [Term]
match_tuple (Tuple v) = Just (toList v)
match_tuple _         = Nothing

match_atom :: Term -> ByteString -> Maybe ByteString
match_atom (Atom n) m | m == n    = Just n
                      | otherwise = Nothing
match_atom _ _ = Nothing

--------------------------------------------------------------------------------
instance Binary Term where
    put (Integer i)
        | i >= 0x00 && i <= 0xFF = do
              putWord8 small_integer_ext
              putWord8 (fromIntegral i)
        | i >= -0x80000000 && i <= 0x7FFFFFFF = do
              putWord8 integer_ext
              putWord32be (fromIntegral i)
        | otherwise =
            -- NOTE: the biggest number presentable is 2^maxBits bits long where
            -- maxBits = 2^32 * 8 = 2^35 - OTOH addressable main memory: 2^64 *
            -- 8 bits = 2^67 bits, even with tomorrows 2048 bit address buses
            -- for 256 bit words this would be at most 2^2056 addressable bits.
            -- large_big_ext allows 2^(2^35) = 2^34359738368 addressable bits ..
            -- hence YES by all practical means 'otherwise' is the correct
            -- function clause guard.
           do let digits = L.unfoldr takeLSB (abs i)
                    where takeLSB x
                            | x == 0     = Nothing
                            | otherwise = Just (fromIntegral (x Data.Bits..&. 0xff), x `shiftR` 8)
              if L.length digits < 256
                then do putWord8 small_big_ext
                        putWord8 (fromIntegral (L.length digits))
                else do putWord8 large_big_ext
                        putWord32be (fromIntegral (L.length digits))
              putWord8 (if i >= 0 then 0 else 1)
              mapM_ putWord8 digits

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

    put (NewReference node' creation ids) = do
        putWord8 new_reference_ext
        putWord16be $ fromIntegral (L.length ids)
        putAtom node'
        putWord8 creation
        mapM_ putWord32be ids
    get = do
        lookAhead getWord8 >>= get'
      where
        get' :: Word8 -> Get Term
        get' tag
            | tag == small_integer_ext =
                  getSmallInteger (Integer . fromIntegral)
            | tag == integer_ext = getInteger (Integer . toInteger . (fromIntegral :: Word32 -> Int32))
            | tag == small_big_ext = getWord8 *> getWord8    >>= getBigInteger . fromIntegral
            | tag == large_big_ext = getWord8 *> getWord32be >>= getBigInteger . fromIntegral
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
putTerm :: (ToTerm t) => t -> Put
putTerm t = do
    putWord8 magicVersion
    put (toTerm t)

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

getBigInteger :: Int -> Get Term
getBigInteger len = mkBigInteger <$> getWord8 <*> getByteString len
  where
    mkBigInteger signByte bs = Integer
        ((if signByte == 0 then 1 else (-1)) * absInt)
        where absInt = BS.foldr' (\b acc -> 256 * acc + fromIntegral b) 0 bs

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

small_integer_ext, integer_ext, float_ext, atom_ext, reference_ext, port_ext, pid_ext
    :: Word8
small_tuple_ext, large_tuple_ext, map_ext, nil_ext, string_ext, list_ext, binary_ext
    :: Word8
small_big_ext, large_big_ext, new_reference_ext, small_atom_ext, fun_ext, new_fun_ext
    :: Word8
export_ext, bit_binary_ext, new_float_ext, atom_utf8_ext, small_atom_utf8_ext
    :: Word8
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

instance Arbitrary Term where
    arbitrary = oneof [ atom <$> scale (`div` 2) arbitraryUnquotedAtom
                      , tuple <$> scale (`div` 2) arbitrary
                      , string <$> scale (`div` 2) arbitraryUnquotedAtom
                      , sized $
                          \qcs -> if qcs > 1
                                  then improperList <$> (getNonEmpty <$> scale (`div` 2) arbitrary)
                                                    <*> scale (`div` 2) arbitrary
                                  else list <$> scale (`div` 2) arbitrary
                      , ref <$> scale smaller arbitraryUnquotedAtom
                            <*> scale smaller arbitrary
                            <*> scale smaller arbitrary
                      , (toTerm :: Pid -> Term) <$> scale smaller arbitrary
                      , float <$> scale smaller arbitrary
                      , (toTerm :: Integer -> Term) <$> scale smaller arbitrary
                      ]

smaller :: (Eq a, Num a) => a -> a
smaller 0 = 0
smaller n = n - 1

arbitraryUnquotedAtom :: Gen CS.ByteString
arbitraryUnquotedAtom =
    CS.pack <$> (listOf1 (elements (['a' .. 'z'] ++ ['_'] ++ ['0' .. '9'])))

instance Arbitrary Pid where
    arbitrary = pid <$> scale smaller arbitraryUnquotedAtom
                    <*> scale smaller arbitrary
                    <*> scale smaller arbitrary
                    <*> scale smaller arbitrary
