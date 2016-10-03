{-# LANGUAGE Rank2Types #-}

module Language.Erlang.HandshakeSpec ( spec ) where

import           Test.Hspec
--import Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as CS
import qualified Data.ByteString.Lazy      as LBS
import           Data.IORef
import           Data.Binary
import           Data.List                 ( nub, sort )

import           Data.IOx
import           Util.BufferedIOx

import           Language.Erlang.NodeData
import           Language.Erlang.Handshake

spec :: Spec
spec = do
    describe "Name" $ do
        it "decode . encode = id" $
            property $ do
                v <- arbitraryBoundedEnum
                f <- (DistributionFlags . nub . sort) <$> listOf arbitraryBoundedEnum
                n <- BS.pack <$> listOf arbitrary
                let a = Name v f n
                return $ (decode . encode) a `shouldBe` a
        it "encodes as expected" $
            encode (Name R6B (DistributionFlags []) "name") `shouldBe`
                withLength16 ("n" `LBS.append` LBS.pack [ 0, 5, 0, 0, 0, 0 ] `LBS.append` "name")

    describe "Status" $ do
        it "decode . encode = id" $
            property $ do
                a <- arbitraryBoundedEnum :: Gen Status
                return $ (decode . encode) a `shouldBe` a
        it "Ok encodes to \"ok\"" $
            encode Ok `shouldBe` withLength16 ("s" `LBS.append` "ok")
        it "OkSimlutaneous encodes to \"ok_simultaneous\"" $
            encode OkSimultaneous `shouldBe`
                withLength16 ("s" `LBS.append` "ok_simultaneous")
        it "Nok encodes to \"nok\"" $
            encode Nok `shouldBe` withLength16 ("s" `LBS.append` "nok")
        it "NotAllowed encodes to \"not_allowed\"" $
            encode NotAllowed `shouldBe` withLength16 ("s" `LBS.append` "not_allowed")
        it "Alive encodes to \"alive\"" $
            encode Alive `shouldBe` withLength16 ("s" `LBS.append` "alive")

    describe "Challenge" $ do
        it "decode . encode = id" $
            property $ do
                v <- arbitraryBoundedEnum
                f <- (DistributionFlags . nub . sort) <$> listOf arbitraryBoundedEnum
                c <- arbitrary
                n <- BS.pack <$> listOf arbitrary
                let a = Challenge v f c n
                return $ (decode . encode) a `shouldBe` a

    describe "ChallengeReply" $ do
        it "decode . encode = id" $
            property $ do
                c <- arbitrary
                d <- BS.pack <$> listOf arbitrary
                let a = ChallengeReply c d
                return $ (decode . encode) a `shouldBe` a

    describe "ChallengeAck" $ do
        it "decode . encode = id" $
            property $ do
                d <- BS.pack <$> listOf arbitrary
                let a = ChallengeAck d
                return $ (decode . encode) a `shouldBe` a

    describe "doConnect and doAccept work together" $ do
        it "correct cookie is accepted" $ do
            let nodeData = NodeData { portNo = 50000
                                    , nodeType = HiddenNode
                                    , protocol = TcpIpV4
                                    , hiVer = R6B
                                    , loVer = R6B
                                    , aliveName = "alive"
                                    , extra = ""
                                    }
                handshakeNode = HandshakeNode { hostName = "localhost.localdomain"
                                              , dFlags = DistributionFlags []
                                              , nodeData
                                              , cookie = "cookie"
                                              }
                name = Name { n_distVer = R6B
                            , n_distFlags = DistributionFlags []
                            , n_nodeName = "alive@localhost.localdomain"
                            }
            her_nodeName <- fromIOx $ do
                                buffer0 <- newBuffer
                                buffer1 <- newBuffer

                                _ <- forkIOx $
                                         doConnect (runPutBuffered buffer0) (runGetBuffered buffer1) handshakeNode name
                                doAccept (runPutBuffered buffer1) (runGetBuffered buffer0) handshakeNode
            her_nodeName `shouldBe`
                "alive@localhost.localdomain"

        it "wrong cookie is rejected" $ do
            let nodeData1 = NodeData { portNo = 50001
                                     , nodeType = HiddenNode
                                     , protocol = TcpIpV4
                                     , hiVer = R6B
                                     , loVer = R6B
                                     , aliveName = "alive1"
                                     , extra = ""
                                     }
                handshakeNode1 = HandshakeNode { hostName = "localhost.localdomain"
                                               , dFlags = DistributionFlags []
                                               , nodeData = nodeData1
                                               , cookie = "cookie1"
                                               }
                name1 = Name { n_distVer = R6B
                             , n_distFlags = DistributionFlags []
                             , n_nodeName = "alive1@localhost.localdomain"
                             }
                nodeData2 = NodeData { portNo = 50002
                                     , nodeType = HiddenNode
                                     , protocol = TcpIpV4
                                     , hiVer = R6B
                                     , loVer = R6B
                                     , aliveName = "alive2"
                                     , extra = ""
                                     }
                handshakeNode2 = HandshakeNode { hostName = "localhost.localdomain"
                                               , dFlags = DistributionFlags []
                                               , nodeData = nodeData2
                                               , cookie = "cookie2"
                                               }
                name2 = Name { n_distVer = R6B
                             , n_distFlags = DistributionFlags []
                             , n_nodeName = "alive2@localhost.localdomain"
                             }
            error_message <- fromIOx $
                                 (do
                                      buffer0 <- newBuffer
                                      buffer1 <- newBuffer

                                      _ <- forkIOx $
                                               doConnect (runPutBuffered buffer0)
                                                         (runGetBuffered buffer1)
                                                         handshakeNode1
                                                         name1
                                      doAccept (runPutBuffered buffer1) (runGetBuffered buffer0) handshakeNode2) `catchX`
                                     (return . CS.pack . show)
            error_message `shouldBe`
                "Cookie mismatch: user error"

withLength16 :: LBS.ByteString -> LBS.ByteString
withLength16 bytes = encode (fromIntegral (LBS.length bytes) :: Word16) `LBS.append` bytes

newtype Buffer = Buffer { bufIO :: IORef BS.ByteString }

instance BufferedIOx Buffer where
    readBuffered = readBuffer
    unreadBuffered = (. LBS.fromStrict) . writeBuffer
    writeBuffered = writeBuffer
    closeBuffered = const (return ())

newBuffer :: IOx Buffer
newBuffer = toIOx $ do
    Buffer <$> newIORef BS.empty

readBuffer :: Buffer -> Int -> IOx BS.ByteString
readBuffer buffer@Buffer{bufIO} len
    | len < 0 = error $ "Bad length: " ++ show len
    | len == 0 = return BS.empty
    | otherwise = toIOx $ do
          atomicModifyIORef' bufIO
                             (\buf -> if BS.null buf
                                      then (BS.empty, Nothing)
                                      else let bufLen = BS.length buf
                                           in
                                               if len > bufLen
                                               then (BS.empty, (Just buf))
                                               else let (buf0, buf1) = BS.splitAt len buf
                                                    in
                                                        (buf1, Just buf0)) >>=
              maybe (fromIOx $ readBuffer buffer len) (return)

writeBuffer :: Buffer -> LBS.ByteString -> IOx ()
writeBuffer Buffer{bufIO} bytes =
    toIOx $ do
        atomicModifyIORef' bufIO (\buf -> (buf `BS.append` (LBS.toStrict bytes), ()))
