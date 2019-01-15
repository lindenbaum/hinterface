{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Foreign.Erlang.TermSpec
  ( spec
  )
where

import           Data.Binary                    ( decode
                                                , encode
                                                )
import           Data.ByteString.Char8          ( )
import qualified Data.ByteString.Lazy          as B
import           Data.Word                      ( )
import           Data.List.NonEmpty             (NonEmpty(..))
import           Foreign.Erlang.Term
import           Test.Hspec
import           Test.QuickCheck
import           Data.Vector                    ( fromList )

spec :: Spec
spec = do
  describe "Pid" $ do
    it "has a Binary instance such that decode is the inverse of encode"
      $ property
      $ \(p :: Pid) -> fromTerm (decode (encode (toTerm p))) `shouldBe` (Just p)
    it "represents all valid Erlang pids" $ property $ \x y z ->
      let p = pid "nodename" x y z
      in  fromTerm (decode (encode (toTerm p))) `shouldBe` (Just p)
  describe "FromTerm/ToTerm" $ do
    it "converts '[a]' back and forth"
      $ property
      $ \(xs :: [Integer]) -> fromTerms (toTerms xs) `shouldBe` Just xs
    it "converts 'Maybe a' back and forth"
      $ property
      $ \(x :: Maybe Bool) -> fromTerm (toTerm x) `shouldBe` Just x
    it "converts 'Either a b' back and forth"
      $ property
      $ \(x :: Either Integer Double) -> fromTerm (toTerm x) `shouldBe` Just x
    it "converts 'NonEmpty a' back and forth"
      $ property
      $ \(h :: Integer) (t :: [Integer]) -> let xs = h :| t in fromTerm (toTerm xs) `shouldBe` Just xs
  describe "Integer"
    $ it "has a Binary instance such that decode is the inverse of encode"
    $ property
    $ \(i :: Integer) ->
        fromTerm (decode (encode (integer i))) `shouldBe` (Just i)
  describe "The largest small_big_ext Integer" $ do
    let i = 2 ^ (8 * 255) - 1
    it "has a Binary instance such that decode is the inverse of encode"
      $          fromTerm (decode (encode (integer i)))
      `shouldBe` (Just i)
    it "is converted to a valid erlang binary"
      $          B.unpack (encode (integer i))
      `shouldBe` [ 110
                 , 255
                 , 0
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 , 255
                 ]
  describe "The smallest large_big_ext Integer"
    $ it "has a Binary instance such that decode is the inverse of encode"
    $ let i = 2 ^ (8 * 255)
      in  fromTerm (decode (encode (integer i))) `shouldBe` (Just i)
  describe "Term" $ do
    it "has a Binary instance such that decode is the inverse of encode"
      $ property
      $ \(t :: Term) -> decode (encode t) `shouldBe` t
    it "has an IsString instance that makes atoms" $ "testatom" `shouldBe` atom
      "testatom"
    describe "Pattern Synonyms" $ do
      it "Tuple3" $ do
        toTerm (Atom "test-atom", integer 1, integer 2)
          `shouldBe` Tuple3 "test-atom" (integer 1) (integer 2)
        (case toTerm (Atom "test-atom", integer 1, integer 2) of
            Tuple3 "test-atom" _ _ -> True
            _                      -> False
          )
          `shouldBe` True
      it "List4" $ do
        List (fromList [integer 0, integer 1, integer 2]) Nil
          `shouldBe` toTerm (List3 (integer 0) (integer 1) (integer 2))
        (case List (fromList [integer 0, integer 1, integer 2]) Nil of
            List3 _ _ _ -> True
            _           -> False
          )
          `shouldBe` True
      it "Map2" $ do
        Map (fromList [MapEntry "k1" "v1", MapEntry "k2" "v2"])
          `shouldBe` toTerm (Map2 ("k1" :=> "v1") ("k2" :=> "v2"))
        (case Map (fromList [MapEntry "k1" "v1", MapEntry "k2" "v2"]) of
            Map2 ("k1" :=> "v1") ("k2" :=> "v2") -> True
            _           -> False
          )
          `shouldBe` True
      it "has an IsList that generates lists" $ do
        ["a1", "a2"]
          `shouldBe` toTerm (List (fromList ["a1", "a2"]) Nil)
