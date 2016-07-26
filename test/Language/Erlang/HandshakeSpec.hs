module Language.Erlang.HandshakeSpec ( spec
                                     )
       where

import Test.Hspec
--import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.List (nub, sort)
import Data.Binary (encode, decode)

import Language.Erlang.NodeData
import Language.Erlang.Handshake

spec :: Spec
spec = do
  describe "Name" $ do
    it "encodes/decodes properly" $ property $ do
      v <- arbitraryBoundedEnum
      f <- DistributionFlags <$> (nub . sort) <$> listOf arbitraryBoundedEnum
      n <- BS.pack <$> listOf arbitrary
      let a = Name v f n
      return $ (decode . encode) a `shouldBe` a
    it "encodes to XXX" $
      encode (Name R6B (DistributionFlags []) "name") `shouldBe` (LBS.pack [0, 11, fromIntegral $ ord 'n', 0, 5, 0, 0, 0, 0] `LBS.append` "name")

  describe "Status" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen Status
      return $ (decode . encode) a `shouldBe` a
    it "Ok encodes to \"ok\"" $
      encode Ok             `shouldBe` (LBS.pack [0,  3, fromIntegral $ ord 's'] `LBS.append` "ok")
    it "OkSimlutaneous encodes to \"ok_simultaneous\"" $
      encode OkSimultaneous `shouldBe` (LBS.pack [0, 16, fromIntegral $ ord 's'] `LBS.append` "ok_simultaneous")
    it "Nok encodes to \"nok\"" $
      encode Nok            `shouldBe` (LBS.pack [0,  4, fromIntegral $ ord 's'] `LBS.append` "nok")
    it "NotAllowed encodes to \"not_allowed\"" $
      encode NotAllowed     `shouldBe` (LBS.pack [0, 12, fromIntegral $ ord 's'] `LBS.append` "not_allowed")
    it "Alive encodes to \"alive\"" $
      encode Alive          `shouldBe` (LBS.pack [0,  6, fromIntegral $ ord 's'] `LBS.append` "alive")

  describe "Challenge" $ do
    it "encodes/decodes properly" $ property $ do
      v <- arbitraryBoundedEnum
      f <- DistributionFlags <$> (nub . sort) <$> listOf arbitraryBoundedEnum
      c <- arbitrary
      n <- BS.pack <$> listOf arbitrary
      let a = Challenge v f c n
      return $ (decode . encode) a `shouldBe` a

  describe "ChallengeReply" $ do
    it "encodes/decodes properly" $ property $ do
      c <- arbitrary
      d <- BS.pack <$> listOf arbitrary
      let a = ChallengeReply c d
      return $ (decode . encode) a `shouldBe` a

  describe "ChallengeAck" $ do
    it "encodes/decodes properly" $ property $ do
      d <- BS.pack <$> listOf arbitrary
      let a = ChallengeAck d
      return $ (decode . encode) a `shouldBe` a
