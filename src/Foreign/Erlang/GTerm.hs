{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Foreign.Erlang.GTerm
    (
    ) where

import           GHC.TypeLits
import           Prelude               hiding ( id, length )
import qualified Prelude               as P ( id )
import           Control.Applicative   ( Alternative(..) )
import           Control.Category      ( (>>>) )
import           Control.Monad         as M ( replicateM )
import           Data.String
import           Data.ByteString       ( ByteString )
import           Data.ByteString.Char8 ( unpack )
import qualified Data.ByteString       as BS ( head, length, tail, unpack, foldr' )
import qualified Data.ByteString.Char8 as CS ( ByteString, pack, unpack )
import           Data.Vector           ( (!), Vector, fromList, toList )
import qualified Data.Vector           as V ( length, replicateM, tail )
import qualified Data.List             as L ( length, unfoldr, length )
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get       hiding ( getBytes )
import           Util.Binary
import           Test.QuickCheck
import           Data.Int
import           Data.Bits             (shiftR, (.&.))
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8, decodeUtf8)

--------------------------------------------------------------------------------

newtype TFloat = TFloat Double
newtype TAtom = TAtom ByteString
data TReference =
          TReference !ByteString !Word32 !Word8
        | TNewReference !ByteString !Word8 ![Word32]
        | TNewerReference
data TPort =
    TPort !ByteString !Word32 !Word8
    | TNewPort
data TPid =
    TPid !ByteString !Word32 !Word32 !Word8
    | TNewPid
newtype TTuple1 t = TTuple1 t

data GTerm t where
    GFloat :: !Double -> GTerm Double
    GInteger :: !Integer -> GTerm Integer
    GAtom :: !ByteString -> GTerm TAtom
    GBinary :: !ByteString -> GTerm ByteString
    GString :: !ByteString -> GTerm Text
    GReference :: TReference -> GTerm TReference
    GPid :: TPid -> GTerm TPid
    GPort :: TPort -> GTerm TPort
    GTuple1 :: !(GTuple '[a1]) -> GTerm (TTuple1 a1)
    GTuple2 :: !(GTuple '[a1, a2])-> GTerm (a1, a2)
    GTuple3 :: !(GTuple '[a1, a2, a3])-> GTerm (a1, a2, a3)
    GTuple  :: !(GTuple (a1 ': a2 ': a3 ': as)) -> GTerm (GTuple (a1 ': a2 ': a3 ': as))
    GNil :: GTerm ()
    GList :: ![GTerm a] -> GTerm [a]

data GTuple ts where
    GTupleNil :: GTuple '[]
    (:|) :: !(GTerm t) -> !(GTuple ts) -> GTuple (t ': ts)

infixr 5 :|

class ToGTerm a where
    toGTerm :: a -> GTerm a

instance ToGTerm Double where toGTerm = GFloat
instance ToGTerm Integer where toGTerm = GInteger
instance ToGTerm TAtom where toGTerm (TAtom a) = GAtom a
instance ToGTerm TReference where toGTerm = GReference
instance ToGTerm TPid where toGTerm = GPid
instance ToGTerm TPort where toGTerm = GPort
instance ToGTerm (GTuple (a1 ': a2 ': a3 ': as)) where toGTerm = GTuple
instance ToGTerm t => ToGTerm (TTuple1 t) where
    toGTerm (TTuple1 t) = GTuple1 (toGTerm t :| GTupleNil)
instance (ToGTerm t1, ToGTerm t2) => ToGTerm (t1, t2) where
    toGTerm (t1, t2) = GTuple2 (toGTerm t1 :| toGTerm t2 :| GTupleNil)
instance (ToGTerm t1, ToGTerm t2, ToGTerm t3) => ToGTerm (t1, t2, t3) where
    toGTerm (t1, t2, t3) = GTuple3 (toGTerm t1 :| toGTerm t2 :| toGTerm t3 :| GTupleNil)
instance ToGTerm () where toGTerm () = GNil
instance ToGTerm a => ToGTerm [a] where toGTerm = GList . fmap toGTerm
instance ToGTerm Text where toGTerm = GString . encodeUtf8
instance ToGTerm ByteString where toGTerm = GBinary

getGTerm :: Int -> (forall a . GTerm a -> b) -> b
getGTerm 0 k = k $ GFloat 123.0
getGTerm 1 k = k $ GInteger 3

xxx f = getGTerm
    f
    ( \k -> case k of
        GFloat _ -> "jo"
        _        -> "ho"
    )

fromGTerm :: GTerm a -> a
fromGTerm (GFloat     t                  ) = t
fromGTerm (GInteger   t                  ) = t
fromGTerm (GAtom      t                  ) = TAtom t
fromGTerm (GBinary    t                  ) = t
fromGTerm (GString    t                  ) = decodeUtf8 t
fromGTerm (GReference t                  ) = t
fromGTerm (GPid       t                  ) = t
fromGTerm (GPort      t                  ) = t
fromGTerm (GTuple     t                  ) = t
fromGTerm (GTuple1    (e1    :|GTupleNil)) = TTuple1 (fromGTerm e1)
fromGTerm (GTuple2    (e1:|e2:|GTupleNil)) = (fromGTerm e1, fromGTerm e2)
fromGTerm (GTuple3 (e1:|e2:|e3:|GTupleNil)) =
    (fromGTerm e1, fromGTerm e2, fromGTerm e3)
fromGTerm GNil       = ()
fromGTerm (GList ts) = fromGTerm <$> ts
