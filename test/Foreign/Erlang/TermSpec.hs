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
    it "represents all valid Erlang pids" $
      withMaxSuccess 40 $ \x y z ->
        let p = pid "nodename" x y z
         in fromTerm (decode (encode (toTerm p))) `shouldBe` Just p
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
        `shouldBe` [ 110,
                     255,
                     0,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255,
                     255
                   ]
  describe "The smallest largeBigIntegerExt Integer" $
    it "has a Binary instance such that decode is the inverse of encode" $
      let i = 2 ^ (8 * 255)
       in fromTerm (decode (encode (integer i))) `shouldBe` Just i
  describe "Term" $ do
    it "has a Binary instance such that decode is the inverse of encode" $
      withMaxSuccess 40 $ \(t :: Term) -> decode (encode t) `shouldBe` t
    it "has an IsString instance that makes atoms" $
      "testatom" `shouldBe` atom "testatom"
    describe "Pattern Synonyms" $ do
      it "Tuple3" $ do
        toTerm (Atom "test-atom", integer 1, integer 2)
          `shouldBe` Tuple3 "test-atom" (integer 1) (integer 2)
        ( case toTerm (Atom "test-atom", integer 1, integer 2) of
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
