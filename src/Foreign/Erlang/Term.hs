{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Foreign.Erlang.Term
  ( ExternalTerm (..),

    -- * Term Format
    Term
      ( ..,
        Tuple2,
        Tuple3,
        Tuple4,
        Tuple5,
        Tuple6,
        Tuple7,
        List1,
        List2,
        List3,
        List4,
        List5,
        List6,
        List7,
        Map1,
        Map2,
        Map3,
        Map4,
        Map5,
        Map6,
        Map7
      ),
    MapEntry (.., (:=>)),
    AtomType (..),

    -- ** Conversion to and from External Term Format
    ToTerm (..),
    toTerms,
    FromTerm (..),
    fromTerms,
    fromTermA,

    -- ** Atoms
    atom,
    atomUtf8,
    smallAtomUtf8,

    -- ** Constructors
    integer,

    -- *** Static numbers
    SInteger (..),
    float,

    -- *** Static atoms
    SAtom (..),
    pid,
    Pid (..),
    tuple,
    Tuple1 (..),
    string,
    list,
    improperList,

    -- ** Recognizers
    isInteger,
    isFloat,
    isAtom,
    isReference,
    isPort,
    isPid,
    isTuple,
    isMap,
    isList,
    isBinary,

    -- ** Accessors
    nodeNameText,
    nodeNameAtomType,
    atomName,
    atomType,
    length,
    element,
    toString,
    toIntegerTerm,

    -- ** Matchers
    matchAtom,
    matchTuple,
  )
where

import Control.Applicative (Alternative (..))
import Control.Category ((>>>))
import Control.DeepSeq
import Control.Monad as M
  ( replicateM,
  )
import Data.Binary
import Data.Binary.Get hiding (getBytes)
import Data.Binary.Put
import Data.Bits
  ( shiftR,
    (.&.),
  )
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString as BS
  ( foldr',
    head,
    length,
    tail,
    unpack,
  )
import Data.Int
import qualified Data.List as L
  ( length,
    unfoldr,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector
  ( Vector,
    (!),
  )
import qualified Data.Vector as V
  ( length,
    replicateM,
    tail,
  )
import GHC.Exts as E
import GHC.Generics
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Test.QuickCheck
import Util.Binary
import Prelude hiding
  ( id,
    length,
  )
import qualified Prelude as P
  ( id,
  )

--------------------------------------------------------------------------------

newtype ExternalTerm = MkExternalTerm {fromExternalTerm :: Term}
  deriving (Eq, Generic, Show, NFData, Arbitrary)

instance Binary ExternalTerm where
  put (MkExternalTerm t) = do
    putWord8 magicVersion
    put t
  get = do
    matchWord8 magicVersion
    MkExternalTerm <$> get

--------------------------------------------------------------------------------

data Term
  = Integer Integer
  | Float Double
  | Atom AtomType Text
  | Reference AtomType Text Word32 Word8
  | NewReference AtomType Text Word8 [Word32]
  | NewerReference AtomType Text Word32 [Word32]
  | Port AtomType Text Word32 Word8
  | NewPort AtomType Text Word32 Word32
  | Pid AtomType Text Word32 Word32 Word8
  | NewPid AtomType Text Word32 Word32 Word32
  | Tuple (Vector Term)
  | Map (Vector MapEntry)
  | Nil
  | String Text
  | List (Vector Term) Term
  | Binary ByteString
  deriving (Eq, Generic)

instance NFData Term

data AtomType
  = OldAtom
  | OldSmallAtom
  | AtomUtf8
  | SmallAtomUtf8
  deriving (Eq, Generic, Ord, Bounded, Enum, Show)

instance NFData AtomType

instance Arbitrary AtomType where
  arbitrary = chooseEnum (minBound, maxBound)

-- ** Pattern Synonyms for 'Term's

pattern Tuple2 :: Term -> Term -> Term
pattern Tuple2 t1 t2 <-
  Tuple (toList -> [t1, t2])
  where
    Tuple2 t1 t2 = Tuple (fromList [t1, t2])

pattern Tuple3 :: Term -> Term -> Term -> Term
pattern Tuple3 t1 t2 t3 <-
  Tuple (toList -> [t1, t2, t3])
  where
    Tuple3 t1 t2 t3 = Tuple (fromList [t1, t2, t3])

pattern Tuple4 :: Term -> Term -> Term -> Term -> Term
pattern Tuple4 t1 t2 t3 t4 <-
  Tuple (toList -> [t1, t2, t3, t4])
  where
    Tuple4 t1 t2 t3 t4 = Tuple (fromList [t1, t2, t3, t4])

pattern Tuple5 :: Term -> Term -> Term -> Term -> Term -> Term
pattern Tuple5 t1 t2 t3 t4 t5 <-
  Tuple (toList -> [t1, t2, t3, t4, t5])
  where
    Tuple5 t1 t2 t3 t4 t5 = Tuple (fromList [t1, t2, t3, t4, t5])

pattern Tuple6 ::
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term
pattern Tuple6 t1 t2 t3 t4 t5 t6 <-
  Tuple (toList -> [t1, t2, t3, t4, t5, t6])
  where
    Tuple6 t1 t2 t3 t4 t5 t6 = Tuple (fromList [t1, t2, t3, t4, t5, t6])

pattern Tuple7 ::
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term
pattern Tuple7 t1 t2 t3 t4 t5 t6 t7 <-
  Tuple (toList -> [t1, t2, t3, t4, t5, t6, t7])
  where
    Tuple7 t1 t2 t3 t4 t5 t6 t7 = Tuple (fromList [t1, t2, t3, t4, t5, t6, t7])

pattern List1 :: Term -> Term
pattern List1 t1 <-
  List (toList -> [t1]) Nil
  where
    List1 t1 = List (fromList [t1]) Nil

pattern List2 :: Term -> Term -> Term
pattern List2 t1 t2 <-
  List (toList -> [t1, t2]) Nil
  where
    List2 t1 t2 = List (fromList [t1, t2]) Nil

pattern List3 :: Term -> Term -> Term -> Term
pattern List3 t1 t2 t3 <-
  List (toList -> [t1, t2, t3]) Nil
  where
    List3 t1 t2 t3 = List (fromList [t1, t2, t3]) Nil

pattern List4 :: Term -> Term -> Term -> Term -> Term
pattern List4 t1 t2 t3 t4 <-
  List (toList -> [t1, t2, t3, t4]) Nil
  where
    List4 t1 t2 t3 t4 = List (fromList [t1, t2, t3, t4]) Nil

pattern List5 :: Term -> Term -> Term -> Term -> Term -> Term
pattern List5 t1 t2 t3 t4 t5 <-
  List (toList -> [t1, t2, t3, t4, t5]) Nil
  where
    List5 t1 t2 t3 t4 t5 = List (fromList [t1, t2, t3, t4, t5]) Nil

pattern List6 ::
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term
pattern List6 t1 t2 t3 t4 t5 t6 <-
  List (toList -> [t1, t2, t3, t4, t5, t6]) Nil
  where
    List6 t1 t2 t3 t4 t5 t6 = List (fromList [t1, t2, t3, t4, t5, t6]) Nil

pattern List7 ::
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term ->
  Term
pattern List7 t1 t2 t3 t4 t5 t6 t7 <-
  List (toList -> [t1, t2, t3, t4, t5, t6, t7]) Nil
  where
    List7 t1 t2 t3 t4 t5 t6 t7 = List (fromList [t1, t2, t3, t4, t5, t6, t7]) Nil

pattern (:=>) :: Term -> Term -> MapEntry
pattern k :=> v = MapEntry k v

pattern Map1 :: MapEntry -> Term
pattern Map1 t1 <-
  Map (toList -> [t1])
  where
    Map1 t1 = Map (fromList [t1])

pattern Map2 :: MapEntry -> MapEntry -> Term
pattern Map2 t1 t2 <-
  Map (toList -> [t1, t2])
  where
    Map2 t1 t2 = Map (fromList [t1, t2])

pattern Map3 :: MapEntry -> MapEntry -> MapEntry -> Term
pattern Map3 t1 t2 t3 <-
  Map (toList -> [t1, t2, t3])
  where
    Map3 t1 t2 t3 = Map (fromList [t1, t2, t3])

pattern Map4 ::
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  Term
pattern Map4 t1 t2 t3 t4 <-
  Map (toList -> [t1, t2, t3, t4])
  where
    Map4 t1 t2 t3 t4 = Map (fromList [t1, t2, t3, t4])

pattern Map5 ::
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  Term
pattern Map5 t1 t2 t3 t4 t5 <-
  Map (toList -> [t1, t2, t3, t4, t5])
  where
    Map5 t1 t2 t3 t4 t5 = Map (fromList [t1, t2, t3, t4, t5])

pattern Map6 ::
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  Term
pattern Map6 t1 t2 t3 t4 t5 t6 <-
  Map (toList -> [t1, t2, t3, t4, t5, t6])
  where
    Map6 t1 t2 t3 t4 t5 t6 = Map (fromList [t1, t2, t3, t4, t5, t6])

pattern Map7 ::
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  MapEntry ->
  Term
pattern Map7 t1 t2 t3 t4 t5 t6 t7 <-
  Map (toList -> [t1, t2, t3, t4, t5, t6, t7])
  where
    Map7 t1 t2 t3 t4 t5 t6 t7 = Map (fromList [t1, t2, t3, t4, t5, t6, t7])

data MapEntry = MapEntry
  { key :: Term,
    value :: Term
  }
  deriving (Eq, Generic)

instance NFData MapEntry

-- number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
instance Ord Term where
  (Integer i) `compare` (Integer i') =
    i `compare` i'
  (Integer i) `compare` (Float d') =
    fromIntegral i `compare` d'
  (Integer _) `compare` _ =
    LT
  (Float d) `compare` (Float d') =
    d `compare` d'
  (Float d) `compare` (Integer i') =
    d `compare` fromIntegral i'
  (Float _) `compare` _ = LT
  (Atom _ a) `compare` (Atom _ a') =
    a `compare` a'
  (Atom _ _) `compare` _ = LT
  (Reference _ node' id creation) `compare` (Reference _ node'' id' creation') =
    (node', id, creation) `compare` (node'', id', creation')
  Reference {} `compare` _ =
    LT
  (NewReference _ node' creation ids) `compare` (NewReference _ node'' creation' ids') =
    (node', creation, ids) `compare` (node'', creation', ids')
  NewReference {} `compare` _ =
    LT
  (NewerReference _ node' id creation) `compare` (NewerReference _ node'' id' creation') =
    (node', id, creation) `compare` (node'', id', creation')
  NewerReference {} `compare` _ =
    LT
  (Port _ node' id creation) `compare` (Port _ node'' id' creation') =
    (node', id, creation) `compare` (node'', id', creation')
  Port {} `compare` _ =
    LT
  (NewPort _ node' id creation) `compare` (NewPort _ node'' id' creation') =
    (node', id, creation) `compare` (node'', id', creation')
  NewPort {} `compare` _ =
    LT
  (NewPid _ node' id' serial creation) `compare` (NewPid _ node'' id'' serial' creation') =
    (node', id', serial, creation) `compare` (node'', id'', serial', creation')
  NewPid {} `compare` _ =
    LT
  (Pid _ node' id serial creation) `compare` (Pid _ node'' id' serial' creation') =
    (node', id, serial, creation) `compare` (node'', id', serial', creation')
  Pid {} `compare` _ =
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

toVector :: Text -> Vector Term
toVector = Text.encodeUtf8 >>> BS.unpack >>> map (fromIntegral >>> Integer) >>> fromList

instance Ord MapEntry where
  MapEntry {key = k, value = v} `compare` MapEntry {key = k', value = v'} =
    (k, v) `compare` (k', v') -- FIXME integer keys are less than float keys

instance Show Term where
  showsPrec _ (Integer i) = shows i
  showsPrec _ (Float d) = shows d
  showsPrec _ (Atom _nt a) = showChar '\'' . showString (Text.unpack a) . showChar '\''
  showsPrec _ (Reference _nt node id _creation) =
    showString "#Ref<" . showString (Text.unpack node) . showChar '.' . shows id . showChar '>'
  showsPrec _ (NewReference _nt nn _creation ids) =
    showString "#NewRef<" . showString (Text.unpack nn) . appEndo (foldMap (\i -> Endo (showChar '.' . shows i)) ids) . showChar '>'
  showsPrec _ (NewerReference _nt nn _creation ids) =
    showString "#NewerRef<" . showString (Text.unpack nn) . appEndo (foldMap (\i -> Endo (showChar '.' . shows i)) ids) . showChar '>'
  showsPrec _ (Port _nt node id _creation) =
    showString "#Port<" . showString (Text.unpack node) . showChar '.' . shows id . showChar '>'
  showsPrec _ (NewPort _nt node id _creation) =
    showString "#NewPort<" . showString (Text.unpack node) . showChar '.' . shows id . showChar '>'
  showsPrec _ (Pid _nt node id serial _creation) =
    showString "#Pid<" . showString (Text.unpack node) . showChar '.' . shows id . showChar '.' . shows serial . showChar '>'
  showsPrec _ (NewPid _nt node id serial c) =
    showString "#NewPid<" . showString (Text.unpack node) . showChar '.' . shows id . showChar '.' . shows serial . showChar '.' . shows c . showChar '>'
  showsPrec _ (Tuple v) = showChar '{' . showsVectorAsList v . showChar '}'
  showsPrec _ (Map e) = showString "#{" . showsVectorAsList e . showChar '}'
  showsPrec _ Nil = showString "[]"
  showsPrec _ (String s) = shows s
  showsPrec _ (List v Nil) = showChar '[' . showsVectorAsList v . showChar ']'
  showsPrec _ (List v t) = showChar '[' . showsVectorAsList v . showChar '|' . shows t . showChar ']'
  showsPrec _ (Binary b) = showString "<<" . showsByteStringAsIntList b . showString ">>"

instance Show MapEntry where
  showsPrec _ MapEntry {key, value} =
    shows key . showString " => " . shows value

showsVectorAsList :: Show a => Vector a -> ShowS
showsVectorAsList v
  | V.length v == 0 = P.id
  | V.length v == 1 = shows (v ! 0)
  | otherwise =
    shows (v ! 0)
      . appEndo (foldMap (\t -> Endo (showChar ',' . shows t)) (V.tail v))

showsByteStringAsIntList :: ByteString -> ShowS
showsByteStringAsIntList b
  | BS.length b == 0 = P.id
  | BS.length b == 1 = shows (BS.head b)
  | otherwise =
    shows (BS.head b)
      . appEndo
        (foldMap (\t -> Endo (showChar ',' . shows t)) (BS.unpack (BS.tail b)))

instance IsString Term where
  fromString = atomUtf8 . Text.pack

instance E.IsList Term where
  type Item Term = Term
  fromList xs = List (fromList xs) Nil
  toList (List xs Nil) = toList xs
  toList _ = []

instance FromTerm Term where
  fromTerm = Just

instance ToTerm Term where
  toTerm = P.id

instance Num Term where
  (Integer x) + (Integer y) = Integer (x + y)
  (Float x) + (Float y) = Float (x + y)
  _ + _ = error "non numeric arguments to (+)"
  (Integer x) * (Integer y) = Integer (x * y)
  (Float x) * (Float y) = Float (x * y)
  _ * _ = error "non numeric arguments to (*)"
  abs (Integer x) = Integer (abs x)
  abs (Float x) = Float (abs x)
  abs _ = error "non numeric arguments to 'abs'"
  signum (Integer x) = Integer (signum x)
  signum (Float x) = Float (signum x)
  signum _ = error "non numeric arguments to 'signum'"
  negate (Integer x) = Integer (negate x)
  negate (Float x) = Float (negate x)
  negate _ = error "non numeric arguments to 'negate'"
  fromInteger = integer

--------------------------------------------------------------------------------

class ToTerm a where
  toTerm :: a -> Term

class FromTerm a where
  fromTerm :: Term -> Maybe a

fromTermA :: (FromTerm a, Alternative m) => Term -> m a
fromTermA t = maybe empty pure (fromTerm t)

instance FromTerm () where
  fromTerm (Tuple ts) | V.length ts == 0 = Just ()
  fromTerm _ = Nothing

instance FromTerm Double where
  fromTerm (Float f) = Just f
  fromTerm _ = Nothing

instance FromTerm Bool where
  fromTerm (Atom _ "true") = Just True
  fromTerm (Atom _ "false") = Just False
  fromTerm _ = Nothing

instance FromTerm Integer where
  fromTerm (Integer i) = Just i
  fromTerm _ = Nothing

instance FromTerm String where
  fromTerm (String s) = Just (Text.unpack s)
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
  fromTerm (Tuple ts)
    | V.length ts == 4 =
      (,,,) <$> fromTerm (ts ! 0)
        <*> fromTerm (ts ! 1)
        <*> fromTerm (ts ! 2)
        <*> fromTerm (ts ! 3)
  fromTerm _ = Nothing

instance (FromTerm a, FromTerm b, FromTerm c, FromTerm d, FromTerm e) => FromTerm (a, b, c, d, e) where
  fromTerm (Tuple ts)
    | V.length ts == 5 =
      (,,,,) <$> fromTerm (ts ! 0)
        <*> fromTerm (ts ! 1)
        <*> fromTerm (ts ! 2)
        <*> fromTerm (ts ! 3)
        <*> fromTerm (ts ! 4)
  fromTerm _ = Nothing

instance FromTerm a => FromTerm (NonEmpty a) where
  fromTerm (List (toList -> xs) Nil) =
    case sequence (fromTerm <$> xs) of
      Just (h : t) -> Just (h :| t)
      _ -> Nothing
  fromTerm _ = Nothing

instance FromTerm a => FromTerm (Maybe a) where
  fromTerm (Tuple2 "ok" x) = Just <$> fromTerm x
  fromTerm "error" = Just Nothing
  fromTerm _ = Nothing

instance (FromTerm a, FromTerm b) => FromTerm (Either a b) where
  fromTerm (Tuple2 "ok" x) = Right <$> fromTerm x
  fromTerm (Tuple2 "error" x) = Left <$> fromTerm x
  fromTerm _ = Nothing

fromTerms :: FromTerm a => Term -> Maybe [a]
fromTerms (List (toList -> xs) Nil) = sequence (fromTerm <$> xs)
fromTerms _ = Nothing

instance ToTerm () where
  toTerm () = tuple []

instance ToTerm Integer where
  toTerm = Integer

instance ToTerm String where
  toTerm = String . Text.pack

instance ToTerm Text where
  toTerm = String

instance ToTerm Bool where
  toTerm True = Atom SmallAtomUtf8 "true"
  toTerm False = Atom SmallAtomUtf8 "false"

instance ToTerm Double where
  toTerm = Float

instance (ToTerm a) => ToTerm (Tuple1 a) where
  toTerm (Tuple1 a) = tuple [toTerm a]

instance (ToTerm a, ToTerm b) => ToTerm (a, b) where
  toTerm (a, b) = tuple [toTerm a, toTerm b]

instance (ToTerm a, ToTerm b, ToTerm c) => ToTerm (a, b, c) where
  toTerm (a, b, c) = tuple [toTerm a, toTerm b, toTerm c]

instance (ToTerm a, ToTerm b, ToTerm c, ToTerm d) => ToTerm (a, b, c, d) where
  toTerm (a, b, c, d) = tuple [toTerm a, toTerm b, toTerm c, toTerm d]

instance (ToTerm a, ToTerm b, ToTerm c, ToTerm d, ToTerm e) => ToTerm (a, b, c, d, e) where
  toTerm (a, b, c, d, e) =
    tuple [toTerm a, toTerm b, toTerm c, toTerm d, toTerm e]

instance ToTerm a => ToTerm (NonEmpty a) where
  toTerm = toTerms . toList

instance ToTerm a => ToTerm (Maybe a) where
  toTerm (Just x) = Tuple2 "ok" (toTerm x)
  toTerm Nothing = "error"

instance (ToTerm a, ToTerm b) => ToTerm (Either a b) where
  toTerm (Left x) = Tuple2 "error" (toTerm x)
  toTerm (Right x) = Tuple2 "ok" (toTerm x)

toTerms :: ToTerm a => [a] -> Term
toTerms xs = List (fromList (toTerm <$> xs)) Nil

--------------------------------------------------------------------------------

-- | Construct an integer
integer ::
  -- | Int
  Integer ->
  Term
integer = Integer

-- | A static/constant number.
data SInteger (n :: Nat) = SInteger

instance (KnownNat n) => Show (SInteger n) where
  showsPrec d s =
    showParen (d > 10) (showString "SInteger '" . showsPrec 11 (natVal s) . showChar '\'')

instance forall (n :: Nat). (KnownNat n) => FromTerm (SInteger n) where
  fromTerm (Integer n') =
    let sn = SInteger
        sn :: SInteger n
     in if n' == natVal sn then Just sn else Nothing
  fromTerm _ = Nothing

instance forall (n :: Nat). (KnownNat n) => ToTerm (SInteger n) where
  toTerm = integer . natVal

-- | Construct a float
float ::
  -- | IEEE float
  Double ->
  Term
float = Float

-- | Construct an atom
atom ::
  -- | AtomName
  Text ->
  Term
atom = Atom OldAtom

atomUtf8 :: Text -> Term
atomUtf8 = Atom AtomUtf8

smallAtomUtf8 :: Text -> Term
smallAtomUtf8 = Atom SmallAtomUtf8

-- | A static/constant atom.
data SAtom (atom :: Symbol) = SAtom

instance (KnownSymbol atom) => Show (SAtom atom) where
  showsPrec d s =
    showParen (d > 10) (showString "SAtom '" . showString (symbolVal s) . showChar '\'')

instance forall (atom :: Symbol). (KnownSymbol atom) => FromTerm (SAtom atom) where
  fromTerm (Atom _ atom') = if atom' == Text.pack (symbolVal (SAtom :: SAtom atom)) then Just SAtom else Nothing
  fromTerm _ = Nothing

instance forall (atom :: Symbol). (KnownSymbol atom) => ToTerm (SAtom atom) where
  toTerm = atom . Text.pack . symbolVal

pid ::
  -- | Node name
  Text ->
  -- | ID
  Word32 ->
  -- | Serial
  Word32 ->
  -- | Creation
  Word32 ->
  Pid
pid = ((.) . (.) . (.) . (.)) MkPid (NewPid AtomUtf8)

newtype Pid = MkPid Term
  deriving (ToTerm, FromTerm, Eq, Ord)

instance Show Pid where
  show (MkPid p) = show p

-- | Construct a tuple
tuple ::
  -- | Elements
  [Term] ->
  Term
tuple = Tuple . fromList

newtype Tuple1 a = Tuple1 a
  deriving (Eq, Ord)

instance (Show a) => Show (Tuple1 a) where
  show (Tuple1 a) = "{" ++ show a ++ "}"

-- map

-- | Construct a list
string ::
  -- | Characters
  Text ->
  Term
string = String

-- | Construct a list
list ::
  -- | Elements
  [Term] ->
  Term
list [] = Nil
list ts = improperList ts Nil

-- | Construct an improper list (if Tail is not Nil)
improperList ::
  -- | Elements
  [Term] ->
  -- | Tail
  Term ->
  Term
improperList [] _ = error "Illegal improper list"
improperList ts t = List (fromList ts) t -- FIXME could check if is string

--------------------------------------------------------------------------------
isInteger,
  isFloat,
  isAtom,
  isReference,
  isPort,
  isPid,
  isTuple,
  isMap,
  isList,
  isBinary ::
    Term -> Bool

-- | Test if term is an integer
isInteger (Integer _) = True
isInteger _ = False

-- | Test if term is a float
isFloat (Float _) = True
isFloat _ = False

-- | Test if term is an atom
isAtom (Atom _ _) = True
isAtom _ = False

-- | Test if term is a reference
isReference Reference {} = True
isReference NewReference {} = True
isReference _ = False

-- | Test if term is a port
isPort Port {} = True
isPort _ = False

-- | Test if term is a pid
isPid Pid {} = True
isPid NewPid {} = True
isPid _ = False

-- | Test if term is a tuple
isTuple (Tuple _) = True
isTuple _ = False

-- | Test if term is a map
isMap (Map _) = True
isMap _ = False

-- | Test if term is a list
isList Nil = True
isList (String _) = True
isList (List _ _) = True
isList _ = False

-- | Test if term is a binary
isBinary (Binary _) = True
isBinary _ = False

--------------------------------------------------------------------------------
nodeNameText :: Term -> Maybe Text
nodeNameText (Reference _nt nn _id _creation) = Just nn
nodeNameText (Port _nt nn _id _creation) = Just nn
nodeNameText (Pid _nt nn _id _serial _creation) = Just nn
nodeNameText (NewPid _nt nn _id _serial _creation) = Just nn
nodeNameText (NewReference _nt nn _creation _ids) = Just nn
nodeNameText _ = Nothing

nodeNameAtomType :: Term -> Maybe AtomType
nodeNameAtomType (Reference nt _nn _id _creation) = Just nt
nodeNameAtomType (Port nt _nn _id _creation) = Just nt
nodeNameAtomType (Pid nt _nn _id _serial _creation) = Just nt
nodeNameAtomType (NewPid nt _nn _id _serial _creation) = Just nt
nodeNameAtomType (NewReference nt _nn _creation _ids) = Just nt
nodeNameAtomType _ = Nothing

atomName :: Term -> Maybe Text
atomName (Atom _ name) = Just name
atomName _ = Nothing

atomType :: Term -> Maybe AtomType
atomType (Atom t _) = Just t
atomType _ = Nothing

length :: Term -> Maybe Int
length (Tuple v) = Just (V.length v)
length (String bs) = Just (BS.length (Text.encodeUtf8 bs))
length (List v Nil) = Just (V.length v)
length _ = Nothing

element :: Int -> Term -> Maybe Term
element n (Tuple v) =
  if V.length v < n then Just (v ! (n - 1)) else Nothing
element _ _ = Nothing

toString :: Term -> Maybe Text
toString (String bs) = Just bs
toString _ = Nothing

toIntegerTerm :: Term -> Maybe Integer
toIntegerTerm (Integer i) = Just i
toIntegerTerm _ = Nothing

matchTuple :: Term -> Maybe [Term]
matchTuple (Tuple v) = Just (toList v)
matchTuple _ = Nothing

matchAtom :: Term -> Text -> Maybe Text
matchAtom (Atom _ n) m
  | m == n = Just n
  | otherwise = Nothing
matchAtom _ _ = Nothing

--------------------------------------------------------------------------------
instance Binary Term where
  put (Integer i)
    | i >= 0x00 && i <= 0xFF = do
      putWord8 smallIntegerExt
      putWord8 (fromIntegral i)
    | i >= -0x80000000 && i <= 0x7FFFFFFF = do
      putWord8 integerExt
      putWord32be (fromIntegral i)
    | otherwise =
      -- NOTE: the biggest number presentable is 2^maxBits bits long where
      -- maxBits = 2^32 * 8 = 2^35 - OTOH addressable main memory: 2^64 *
      -- 8 bits = 2^67 bits, even with tomorrows 2048 bit address buses
      -- for 256 bit words this would be at most 2^2056 addressable bits.
      -- largeBigIntegerExt allows 2^(2^35) = 2^34359738368 addressable bits ..
      -- hence YES by all practical means 'otherwise' is the correct
      -- function clause guard.
      do
        let digits = L.unfoldr takeLSB (abs i)
              where
                takeLSB x
                  | x == 0 = Nothing
                  | otherwise = Just (fromIntegral (x Data.Bits..&. 0xff), x `shiftR` 8)
        if L.length digits < 256
          then do
            putWord8 smallBigIntegerExt
            putWord8 (fromIntegral (L.length digits))
          else do
            putWord8 largeBigIntegerExt
            putWord32be (fromIntegral (L.length digits))
        putWord8 (if i >= 0 then 0 else 1)
        mapM_ putWord8 digits
  put (Float d) = do
    putWord8 newFloatExt
    putDoublebe d
  put (Atom OldAtom n) = do
      putWord8 atomExt
      putLength16beText n
  put (Atom OldSmallAtom n) = do
      putWord8 smallAtomExt
      putLength8Text n
  put (Atom AtomUtf8 n) = do
      putWord8 atomUtf8Ext
      putLength16beText n
  put (Atom SmallAtomUtf8 n) = do
    putWord8 smallAtomUtf8Ext
    putLength8Text n
  put (Reference nt node id creation) = do
    putWord8 referenceExt
    put (Atom nt node)
    putWord32be id
    putWord8 creation
  put (NewReference nt node creation ids) = do
    putWord8 newReferenceExt
    putWord16be $ fromIntegral (L.length ids)
    put (Atom nt node)
    putWord8 creation
    mapM_ putWord32be ids
  put (NewerReference nt node creation ids) = do
    putWord8 newerReferenceExt
    putWord16be $ fromIntegral (L.length ids)
    put (Atom nt node)
    putWord32be creation
    mapM_ putWord32be ids
  put (Port nt node id creation) = do
    putWord8 portExt
    put (Atom nt node)
    putWord32be id
    putWord8 creation
  put (NewPort nt node id creation) = do
    putWord8 newPortExt
    put (Atom nt node)
    putWord32be id
    putWord32be creation
  put (Pid nt node id serial creation) = do
    putWord8 pidExt
    put (Atom nt node)
    putWord32be id
    putWord32be serial
    putWord8 creation
  put (NewPid nt node id serial creation) = do
    putWord8 newPidExt
    put (Atom nt node)
    putWord32be id
    putWord32be serial
    putWord32be creation
  put (Tuple v)
    | V.length v < 256 = do
      putWord8 smallTupleExt
      putWord8 $ fromIntegral (V.length v)
      mapM_ put v
    | otherwise = do
      putWord8 largeTupleExt
      putWord32be $ fromIntegral (V.length v)
      mapM_ put v
  put (Map e) = do
    putWord8 mapExt
    putWord32be $ fromIntegral (V.length e)
    mapM_ put e
  put Nil = putWord8 nilExt
  put (String s) = do
    putWord8 stringExt
    putLength16beText s
  put (List v t) = do
    putWord8 listExt
    putWord32be $ fromIntegral (V.length v)
    mapM_ put v
    put t
  put (Binary b) = do
    putWord8 binaryExt
    putLength32beByteString b

  get = getWord8 >>= get'
    where
      get' :: Word8 -> Get Term
      get' tag
        | tag == smallIntegerExt = Integer . fromIntegral <$> getWord8
        | tag == integerExt = Integer . toInteger . (fromIntegral :: Word32 -> Int32) <$> getWord32be
        | tag == smallBigIntegerExt = getWord8 >>= getBigInteger . fromIntegral
        | tag == largeBigIntegerExt = getWord32be >>= getBigInteger . fromIntegral
        | tag == newFloatExt = Float <$> getDoublebe
        | tag == atomExt = Atom OldAtom <$> getLength16beText
        | tag == smallAtomExt = Atom OldSmallAtom <$> getLength8Text
        | tag == atomUtf8Ext = Atom AtomUtf8 <$> getLength16beText
        | tag == smallAtomUtf8Ext = Atom SmallAtomUtf8 <$> getLength8Text
        | tag == portExt = uncurry Port <$> getNodeNameAtom <*> getWord32be <*> getWord8
        | tag == newPortExt = uncurry NewPort <$> getNodeNameAtom <*> getWord32be <*> getWord32be
        | tag == pidExt = uncurry Pid <$> getNodeNameAtom <*> getWord32be <*> getWord32be <*> getWord8
        | tag == newPidExt = uncurry NewPid <$> getNodeNameAtom <*> getWord32be <*> getWord32be <*> getWord32be
        | tag == smallTupleExt = Tuple <$> (getWord8 >>= _getVector . fromIntegral)
        | tag == largeTupleExt = Tuple <$> (getWord32be >>= _getVector . fromIntegral)
        | tag == mapExt = Map <$> (getWord32be >>= _getVector . fromIntegral)
        | tag == nilExt = pure Nil
        | tag == stringExt = String <$> getLength16beText
        | tag == listExt = List <$> (getWord32be >>= _getVector . fromIntegral) <*> get
        | tag == binaryExt = Binary <$> getLength32beByteString
        | tag == referenceExt =
          uncurry Reference <$> getNodeNameAtom <*> getWord32be <*> getWord8
        | tag == newReferenceExt = do
          len <- getWord16be
          (nt, nn) <- getNodeNameAtom
          NewReference nt nn <$> getWord8 <*> _getList (fromIntegral len)
        | tag == newerReferenceExt = do
          len <- getWord16be
          uncurry NewerReference <$> getNodeNameAtom <*> getWord32be <*> _getList (fromIntegral len)
        | otherwise = fail $ "Unsupported tag: " ++ show tag

instance Binary MapEntry where
  put MapEntry {key, value} = do
    put key
    put value
  get = MapEntry <$> get <*> get

getNodeNameAtom :: Get (AtomType, Text)
getNodeNameAtom =
  get >>= \case
    Atom nt nn -> return (nt, nn)
    other -> fail ("expected a node name atom, but got: " ++ show other)

--------------------------------------------------------------------------------

putSmallAtomUtf8 :: HasCallStack => Text -> Put
putSmallAtomUtf8 a = do
  putWord8 smallAtomUtf8Ext
  putLength8Text a

--------------------------------------------------------------------------------

getBigInteger :: HasCallStack => Int -> Get Term
getBigInteger len = mkBigInteger <$> getWord8 <*> getByteString len
  where
    mkBigInteger signByte bs =
      Integer
        ((if signByte == 0 then 1 else (-1)) * absInt)
      where
        absInt = BS.foldr' (\b acc -> 256 * acc + fromIntegral b) 0 bs

--------------------------------------------------------------------------------
_getVector :: HasCallStack => Binary a => Int -> Get (Vector a)
_getVector len = V.replicateM len get

_getList :: HasCallStack => Binary a => Int -> Get [a]
_getList len = M.replicateM len get

--------------------------------------------------------------------------------
magicVersion :: Word8
magicVersion = 131

smallIntegerExt,
  integerExt,
  floatExt,
  atomExt,
  referenceExt,
  newReferenceExt,
  newerReferenceExt,
  portExt,
  newPortExt,
  newPidExt,
  pidExt,
  smallTupleExt,
  largeTupleExt,
  mapExt,
  nilExt,
  stringExt,
  listExt,
  binaryExt,
  newFunctionExt,
  smallBigIntegerExt,
  largeBigIntegerExt,
  smallAtomExt,
  functionExt,
  exportExt,
  bitBinaryExt,
  newFloatExt,
  atomUtf8Ext,
  smallAtomUtf8Ext ::
    Word8
smallIntegerExt = 97
integerExt = 98
floatExt = 99
atomExt = 100
smallAtomExt = 115
atomUtf8Ext = 118
smallAtomUtf8Ext = 119
referenceExt = 101
newReferenceExt = 114
newerReferenceExt = 90
portExt = 102
newPortExt = 89
pidExt = 103
newPidExt = 88
smallTupleExt = 104
largeTupleExt = 105
mapExt = 116
nilExt = 106
stringExt = 107
listExt = 108
binaryExt = 109
smallBigIntegerExt = 110
largeBigIntegerExt = 111
functionExt = 117
newFunctionExt = 112
exportExt = 113
bitBinaryExt = 77
newFloatExt = 70

instance Arbitrary Term where
  arbitrary =
    oneof
      [ do
          nt <- arbitrary
          n <- scale (`div` 2) arbitraryUnquotedAtom
          let maxLen = case nt of
                OldSmallAtom -> 255
                SmallAtomUtf8 -> 255
                _ -> Text.length n
          pure (Atom nt (Text.take maxLen n)),
        tuple <$> scale (`div` 2) arbitrary,
        string <$> scale (`div` 2) arbitraryUnquotedAtom,
        sized $
          \qcs ->
            if qcs > 1
              then
                improperList <$> (getNonEmpty <$> scale (`div` 2) arbitrary)
                  <*> scale (`div` 2) arbitrary
              else list <$> scale (`div` 2) arbitrary,
        Reference <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        NewReference <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        NewerReference <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        Pid <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        NewPid <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        Port <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        NewPort <$> arbitrary
          <*> scale smaller arbitraryUnquotedAtom
          <*> scale smaller arbitrary
          <*> scale smaller arbitrary,
        float <$> scale smaller arbitrary,
        (toTerm :: Integer -> Term) <$> scale smaller arbitrary,
        Binary . pack <$> arbitrary
      ]

smaller :: (Eq a, Num a) => a -> a
smaller 0 = 0
smaller n = n - 1

arbitraryUnquotedAtom :: Gen Text
arbitraryUnquotedAtom =
  Text.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['_'] ++ ['0' .. '9']))

instance Arbitrary Pid where
  arbitrary =
    pid <$> scale smaller arbitraryUnquotedAtom
      <*> scale smaller arbitrary
      <*> scale smaller arbitrary
      <*> scale smaller arbitrary
