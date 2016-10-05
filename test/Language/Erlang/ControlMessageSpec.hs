{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Erlang.ControlMessageSpec ( spec ) where

import           Data.Binary                    (decode, encode)
import           Language.Erlang.ControlMessage
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  describe "ControlMessage" $
    it "decode . encode = id" $
      property $ \ (a :: ControlMessage) ->
                   decode (encode a) `shouldBe` a
