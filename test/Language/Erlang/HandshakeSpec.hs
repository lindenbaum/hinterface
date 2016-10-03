{-# LANGUAGE Rank2Types #-}

module Language.Erlang.HandshakeSpec ( spec ) where

import           Test.Hspec
--import Test.Hspec.QuickCheck
import           Test.QuickCheck

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.IORef
import           Data.Binary
import           Data.List                 ( nub, sort )

import           Data.IOx
import           Util.Binary

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
                                         doConnect (runPutBuffer buffer0) (runGetBuffer buffer1) handshakeNode name
                                doAccept (runPutBuffer buffer1) (runGetBuffer buffer0) handshakeNode
            her_nodeName `shouldBe`
                "alive@localhost.localdomain"

newtype Buffer = Buffer { bufIO :: IORef BS.ByteString }

newBuffer :: IOx Buffer
newBuffer = toIOx $ do
    Buffer <$> newIORef BS.empty

writeBuffer :: Buffer -> LBS.ByteString -> IOx ()
writeBuffer Buffer{bufIO} bytes =
    toIOx $ do
        atomicModifyIORef' bufIO (\buf -> (buf `BS.append` (LBS.toStrict bytes), ()))

readBuffer :: Buffer -> Int -> IOx BS.ByteString
readBuffer buffer@Buffer{bufIO} len
    | len < 0 = error $ "Bad length: " ++ show len
    | len == 0 = return BS.empty
    | otherwise = toIOx $ do
          res <- atomicModifyIORef' bufIO
                                    (\buf -> if BS.null buf
                                             then (BS.empty, Nothing)
                                             else let bufLen = BS.length buf
                                                  in
                                                      if len > bufLen
                                                      then (BS.empty, (Just buf))
                                                      else let (buf0, buf1) = BS.splitAt len buf
                                                           in
                                                               (buf1, Just buf0))
          case res of
              Nothing -> do
                  fromIOx $ readBuffer buffer len
              Just (ret) -> do
                  return ret

runGetBuffer'' :: Buffer -> Get a -> IOx (Either String a)
runGetBuffer'' = runGetA <$> readBuffer <*> unreadBuffer
  where
    unreadBuffer = (. LBS.fromStrict) . writeBuffer

runGetBuffer' :: Buffer -> Get a -> IOx a
runGetBuffer' s g = (runGetBuffer'' s g) >>= either (errorX userErrorType) (return)

runGetBuffer :: (Binary a) => Buffer -> IOx a
runGetBuffer = flip runGetBuffer' get

runPutBuffer' :: Buffer -> Put -> IOx ()
runPutBuffer' = runPutA <$> writeBuffer

runPutBuffer :: (Binary a) => Buffer -> a -> IOx ()
runPutBuffer = (. put) . runPutBuffer'

withLength16 :: LBS.ByteString -> LBS.ByteString
withLength16 bytes = encode (fromIntegral (LBS.length bytes) :: Word16) `LBS.append` bytes
