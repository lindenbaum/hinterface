{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Erlang.TermSpec ( spec ) where

import           Data.Binary           (decode, encode)
import           Data.ByteString.Char8 ()
import           Data.Word             ()
import           Language.Erlang.Term
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Pid"
    $ do it "has a Binary instance such that decode is the inverse of encode"
           $ property
           $ \ (p :: Pid) ->
               fromTerm (decode (encode (toTerm p))) `shouldBe` (Just p)
         it "represents all valid Erlang pids"
           $ property
           $ \ x y z ->
               let p = pid "nodename" x y z
               in fromTerm (decode (encode (toTerm p))) `shouldBe` (Just p)
  describe "Integer"
    $ it "has a Binary instance such that decode is the inverse of encode"
    $ property
    $ \ (i :: Integer) ->
        fromTerm (decode (encode (integer i))) `shouldBe` (Just i)
  describe "Term"
    $ it "has a Binary instance such that decode is the inverse of encode"
    $ property
    $ \ (t :: Term) ->
        decode (encode t) `shouldBe` t
