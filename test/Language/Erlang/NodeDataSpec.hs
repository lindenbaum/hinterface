module Language.Erlang.NodeDataSpec ( spec
                                    )
       where

import Test.Hspec
--import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Word
import Data.List (nub, sort)
import Data.Binary (encode, decode)

import Language.Erlang.NodeData

spec :: Spec
spec = do
  describe "NodeType" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen NodeType
      return $ (decode . encode) a `shouldBe` a
    it "NormalNode encodes to 77" $
      encode NormalNode `shouldBe` encode (77 :: Word8)
    it "HiddenNode encodes to 72" $
      encode HiddenNode `shouldBe` encode (72 :: Word8)


  describe "NodeProtocol" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen NodeProtocol
      return $ (decode . encode) a `shouldBe` a
    it "TcpIpV4 encodes to 0" $
      encode TcpIpV4 `shouldBe` encode (0 :: Word8)

  describe "DistributionVersion" $ do
    it "encodes/decodes properly" $ property $ do
      a <- arbitraryBoundedEnum :: Gen DistributionVersion
      return $ (decode . encode) a `shouldBe` a
    it "R4 encodes to 1" $
      encode R4 `shouldBe` encode (1 :: Word16)
    it "R5C encodes to 3" $
      encode R5C `shouldBe` encode (3 :: Word16)
    it "R6 encodes to 4" $
      encode R6 `shouldBe` encode (4 :: Word16)
    it "R6B encodes to 5" $
      encode R6B `shouldBe` encode (5 :: Word16)

  describe "DistributionFlags" $ do
    it "encodes/decodes properly" $ property $ do
      a <- DistributionFlags <$> (nub . sort) <$> listOf arbitraryBoundedEnum :: Gen DistributionFlags
      return $ (decode . encode) a `shouldBe` a
    it "[] encodes to 0x00000" $
      encode (DistributionFlags []) `shouldBe` encode (0x00000 :: Word32)
    it "[PUBLISHED] encodes to 0x00001" $
      encode (DistributionFlags [PUBLISHED]) `shouldBe` encode (0x00001 :: Word32)
    it "[ATOM_CACHE] encodes to 0x00002" $
      encode (DistributionFlags [ATOM_CACHE]) `shouldBe` encode (0x00002 :: Word32)
    it "[EXTENDED_REFERENCES] encodes to 0x00004" $
      encode (DistributionFlags [EXTENDED_REFERENCES]) `shouldBe` encode (0x00004 :: Word32)
    it "[DIST_MONITOR] encodes to 0x00008" $
      encode (DistributionFlags [DIST_MONITOR]) `shouldBe` encode (0x00008 :: Word32)
    it "[FUN_TAGS] encodes to 0x00010" $
      encode (DistributionFlags [FUN_TAGS]) `shouldBe` encode (0x00010 :: Word32)
    it "[DIST_MONITOR_NAME] encodes to 0x00020" $
      encode (DistributionFlags [DIST_MONITOR_NAME]) `shouldBe` encode (0x00020 :: Word32)
    it "[HIDDEN_ATOM_CACHE] encodes to 0x00040" $
      encode (DistributionFlags [HIDDEN_ATOM_CACHE]) `shouldBe` encode (0x00040 :: Word32)
    it "[NEW_FUN_TAGS] encodes to 0x00080" $
      encode (DistributionFlags [NEW_FUN_TAGS]) `shouldBe` encode (0x00080 :: Word32)
    it "[EXTENDED_PIDS_PORTS] encodes to 0x00100" $
      encode (DistributionFlags [EXTENDED_PIDS_PORTS]) `shouldBe` encode (0x00100 :: Word32)
    it "[EXPORT_PTR_TAG] encodes to 0x00200" $
      encode (DistributionFlags [EXPORT_PTR_TAG]) `shouldBe` encode (0x00200 :: Word32)
    it "[BIT_BINARIES] encodes to 0x00400" $
      encode (DistributionFlags [BIT_BINARIES]) `shouldBe` encode (0x00400 :: Word32)
    it "[NEW_FLOATS] encodes to 0x00800" $
      encode (DistributionFlags [NEW_FLOATS]) `shouldBe` encode (0x00800 :: Word32)
    it "[UNICODE_IO] encodes to 0x01000" $
      encode (DistributionFlags [UNICODE_IO]) `shouldBe` encode (0x01000 :: Word32)
    it "[DIST_HDR_ATOM_CACHE] encodes to 0x02000" $
      encode (DistributionFlags [DIST_HDR_ATOM_CACHE]) `shouldBe` encode (0x02000 :: Word32)
    it "[SMALL_ATOM_TAGS] encodes to 0x04000" $
      encode (DistributionFlags [SMALL_ATOM_TAGS]) `shouldBe` encode (0x04000 :: Word32)
    it "[UTF8_ATOMS] encodes to 0x10000" $
      encode (DistributionFlags [UTF8_ATOMS]) `shouldBe` encode (0x10000 :: Word32)
