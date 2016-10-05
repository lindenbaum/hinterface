{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Erlang.TermSpec ( spec ) where

import           Data.Binary          (decode, encode)
import           Language.Erlang.Term
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  do describe "Pid" $
       do it "encode . decode = id" $ property $ \ (p :: Pid) ->
            fromTerm (decode (encode (toTerm p))) `shouldBe` (Just p)
     describe "Integer" $
       do it "encode . decode = id" $ property $ \ (i :: Integer) ->
            fromTerm (decode (encode (integer i))) `shouldBe` (Just i)
     describe "Term" $
       do it "encode . decode = id" $ property $ \ (t :: Term) ->
            decode (encode t) `shouldBe` t
