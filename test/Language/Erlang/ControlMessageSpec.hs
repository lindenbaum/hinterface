{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Erlang.ControlMessageSpec ( spec ) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.Binary                    ( decode, encode )

import           Language.Erlang.ControlMessage
import           Language.Erlang.Term

spec :: Spec
spec = do
    describe "ControlMessage" $ do
        it "decode . encode = id" $
            property $ do
                a <- arbitrary :: Gen ControlMessage
                return $ (decode . encode) a `shouldBe` a

instance Arbitrary ControlMessage where
    arbitrary = oneof [ pure TICK
                      , LINK <$> arbitrary <*> arbitrary
                      , SEND <$> arbitrary <*> arbitrary
                      , EXIT <$> arbitrary <*> arbitrary <*> arbitrary
                      , UNLINK <$> arbitrary <*> arbitrary
                      , pure NODE_LINK
                      , REG_SEND <$> arbitrary <*> arbitrary <*> arbitrary
                      , GROUP_LEADER <$> arbitrary <*> arbitrary
                      , EXIT2 <$> arbitrary <*> arbitrary <*> arbitrary
                      ]

instance Arbitrary Term where
    arbitrary = oneof [ pure (atom "name") ]
