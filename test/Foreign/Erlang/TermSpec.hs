{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Foreign.Erlang.TermSpec
  ( spec,
  )
where

import Data.Binary
  ( decode,
    encode,
  )
import qualified Data.ByteString.Lazy as B
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (fromList)
import Data.Word ()
import Foreign.Erlang.Term
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- import           Debug.Trace

traceShowIdBS :: B.ByteString -> B.ByteString
traceShowIdBS bs = bs

-- traceShowIdBS bs = traceShow (B.unpack bs) bs

spec :: HasCallStack => Spec
spec = do
  describe "Binary instances of arbitrary ExternalTerm" $
    it "decode . encode = id" $
      withMaxSuccess 40 $ \(t :: ExternalTerm) -> decode (traceShowIdBS (encode t)) === t
  describe "Binary instances of arbitrary Term" $
    it "decode . encode = id" $
      withMaxSuccess 40 $ \(MkExternalTerm t) -> decode (traceShowIdBS (encode t)) === t
  describe "Pid" $ do
    it "has a Binary instance such that decode is the inverse of encode" $
      withMaxSuccess 40 $ \(p :: Pid) ->
        fromTerm (decode (encode (toTerm p))) `shouldBe` Just p
    it "represents all valid Erlang pids - old pids" $
      withMaxSuccess 40 $ \w x y z ->
        let p = Pid w "nodename" x y z
         in fromTerm (decode (encode (toTerm p))) `shouldBe` Just p
  describe "NewPid" $ do
    it "can be shown" $
      show (NewPid SmallAtomUtf8 "nodename" 1 2 3) `shouldContain` "nodename"
    it "can be shown as term" $
      show (toTerm (NewPid SmallAtomUtf8 "nodename" 1 2 3)) `shouldContain` "nodename"
    it "can be compared" $
      NewPid SmallAtomUtf8 "nodename" 1 2 3 >= NewPid SmallAtomUtf8 "modename" 0 1 2 `shouldBe` True
    it "satisfies is_pid" $
      isPid (toTerm (NewPid SmallAtomUtf8 "nodename" 1 2 3)) `shouldBe` True
    it "has a node part" $
      nodeNameText (toTerm (NewPid SmallAtomUtf8 "nodename" 1 2 3)) `shouldBe` Just "nodename"
    it "has a Binary instance such that decode is the inverse of encode" $
      withMaxSuccess 40 $ \(p :: Pid) ->
        fromTerm (decode (encode (toTerm p))) `shouldBe` Just p
    it "represents all valid Erlang pids" $
      withMaxSuccess 40 $ \w x y z ->
        let p = NewPid w "nodename" x y z
         in fromTerm (decode (encode (toTerm p))) `shouldBe` Just p

  describe "NewerReference" $ do
    describe "binary instance" $ do
      let tagB = B.pack [90]
          lenB0 = B.pack [0, 0]
          lenB1 = B.pack [0, 1]
          lenB2 = B.pack [0, 2]
          lenB3 = B.pack [0, 3]
          node = "test@local"
          nodeTxtLen = Text.length node
          nodeBSmall = B.pack [119, fromIntegral nodeTxtLen] <> B.fromStrict (Text.encodeUtf8 node)
          nodeB = B.pack [118, fromIntegral (nodeTxtLen `div` 255), fromIntegral (nodeTxtLen `mod` 255)] <> B.fromStrict (Text.encodeUtf8 node)
          creationB = B.pack [1, 2, 3, 4]
          idB0 = B.pack []
          idB1 = B.pack [2, 3, 4, 5]
          idB2 = B.pack [2, 3, 4, 5, 3, 4, 5, 6]
          idB3 = B.pack [2, 3, 4, 5, 3, 4, 5, 6, 4, 5, 6, 7]
          t0 = NewerReference SmallAtomUtf8 node 0x01020304 []
          b0 = tagB <> lenB0 <> nodeBSmall <> creationB <> idB0
          t1 = NewerReference SmallAtomUtf8 node 0x01020304 [0x02030405]
          b1 = tagB <> lenB1 <> nodeBSmall <> creationB <> idB1
          t2 = NewerReference SmallAtomUtf8 node 0x01020304 [0x02030405, 0x03040506]
          b2 = tagB <> lenB2 <> nodeBSmall <> creationB <> idB2
          t3 = NewerReference SmallAtomUtf8 node 0x01020304 [0x02030405, 0x03040506, 0x04050607]
          b3 = tagB <> lenB3 <> nodeBSmall <> creationB <> idB3
      it "can be shown" $ do
        show t0 `shouldNotBe` ""
        show t1 `shouldNotBe` ""
        show t2 `shouldNotBe` ""
      it "can be compared" $ do
        t0 `shouldBe` t0
        t1 `shouldBe` t1
        t2 `shouldBe` t2
        t3 `shouldBe` t3
      it "nodeBSmall decodes correctly" $
        decode nodeBSmall `shouldBe` Atom SmallAtomUtf8 node
      it "nodeBSmall encodes correctly" $
         encode (Atom SmallAtomUtf8 node) `shouldBe` nodeBSmall
      it "nodeB decodes correctly" $
        decode nodeB `shouldBe` Atom AtomUtf8 node
      it "nodeB encodes correctly" $
         encode (Atom AtomUtf8 node) `shouldBe` nodeB
      it "serializes with tag 90" $
        decode (tagB <> lenB0 <> nodeBSmall <> creationB <> idB0)
          `shouldBe` NewerReference SmallAtomUtf8 node 0x01020304 []
      it "deserializes with tag 90" $
        encode (NewerReference AtomUtf8 node 0x01020304 [])
          `shouldBe` (tagB <> lenB0 <> nodeB <> creationB <> idB0)
      it "can be parsed from a smallAtomUtf8 node" $
        decode (tagB <> lenB0 <> nodeBSmall <> creationB <> idB0)
          `shouldBe` NewerReference SmallAtomUtf8 node 0x01020304 []
      it "can be parsed with a atomUtf8 node" $
        decode (tagB <> lenB0 <> nodeB <> creationB <> idB0)
          `shouldBe` NewerReference AtomUtf8 node 0x01020304 []
      it "can have an ID field with length 0,1,2 and 3" $ do
        let check t b = do
              decode b `shouldBe` t
              encode t `shouldBe` b
              encode (decode b :: Term) `shouldBe` b
              decode (encode t) `shouldBe` t
        check t0 b0
        check t1 b1
        check t2 b2
        check t3 b3
  describe "FromTerm/ToTerm" $ do
    it "converts '[Integer]' back and forth" $
      withMaxSuccess 40 $ \(xs :: [Integer]) -> fromTerms (toTerms xs) `shouldBe` Just xs
    it "converts 'Maybe Bool' back and forth" $
      withMaxSuccess 40 $ \(x :: Maybe Bool) -> fromTerm (toTerm x) `shouldBe` Just x
    it "converts 'Either a b' back and forth" $
      withMaxSuccess 40 $ \(x :: Either Integer Double) ->
        fromTerm (toTerm x) `shouldBe` Just x
    it "converts 'NonEmpty a' back and forth" $
      withMaxSuccess 40 $ \(h :: Integer) (t :: [Integer]) ->
        let xs = h :| t
         in fromTerm (toTerm xs) `shouldBe` Just xs
  describe "Integer" $
    it "has a Binary instance such that decode is the inverse of encode" $
      withMaxSuccess 40 $ \(i :: Integer) ->
        fromTerm (decode (encode (integer i))) `shouldBe` Just i
  describe "The largest smallBigIntegerExt Integer" $ do
    let i = 2 ^ (8 * 255) - 1
    it "has a Binary instance such that decode is the inverse of encode" $
      fromTerm (decode (encode (integer i))) `shouldBe` Just i
    it "is converted to a valid erlang binary" $
      B.unpack (encode (integer i))
        `shouldBe` (110 : 255 :  0: replicate 255 255)
  describe "The smallest largeBigIntegerExt Integer" $
    it "has a Binary instance such that decode is the inverse of encode" $
      let i = 2 ^ (8 * 255)
       in fromTerm (decode (encode (integer i))) `shouldBe` Just i
  describe "Term" $ do
    it "has a Binary instance such that encode . decode . encode == encode" $
      withMaxSuccess 40 $ \(t :: Term) -> encode (decode (encode t) :: Term) `shouldBe` encode t
    it "has a Binary instance such that decode is the inverse of encode" $
      withMaxSuccess 40 $ \(t :: Term) -> decode (encode t) `shouldBe` t
    it "has an IsString instance that makes atoms" $
      atomName "testatom" `shouldBe` Just "testatom"
    describe "Pattern Synonyms" $ do
      it "Tuple3" $ do
        toTerm ("test-atom" :: Term, integer 1, integer 2)
          `shouldBe` Tuple3 "test-atom" (integer 1) (integer 2)
        ( case toTerm (Atom AtomUtf8 "test-atom", integer 1, integer 2) of
            Tuple3 "test-atom" _ _ -> True
            _ -> False
          )
          `shouldBe` True
      it "List4" $ do
        List (fromList [integer 0, integer 1, integer 2]) Nil
          `shouldBe` toTerm (List3 (integer 0) (integer 1) (integer 2))
        ( case List (fromList [integer 0, integer 1, integer 2]) Nil of
            List3 {} -> True
            _ -> False
          )
          `shouldBe` True
      it "Map2" $ do
        Map (fromList [MapEntry "k1" "v1", MapEntry "k2" "v2"])
          `shouldBe` toTerm (Map2 ("k1" :=> "v1") ("k2" :=> "v2"))
        ( case Map (fromList [MapEntry "k1" "v1", MapEntry "k2" "v2"]) of
            Map2 ("k1" :=> "v1") ("k2" :=> "v2") -> True
            _ -> False
          )
          `shouldBe` True
      it "has an IsList that generates lists" $
        ["a1", "a2"] `shouldBe` toTerm (List (fromList ["a1", "a2"]) Nil)
