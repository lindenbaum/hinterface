{-# LANGUAGE ScopedTypeVariables #-}

module Language.Erlang.ControlMessageSpec ( spec ) where

import           Test.Hspec
import           Test.QuickCheck

import qualified Data.ByteString.Lazy           as LBS
import           Data.Binary                    ( decode, encode )

import           Language.Erlang.ControlMessage
import           Language.Erlang.Term

spec :: Spec
spec = describe "ControlMessage" $ do
    it "decode . encode = id" $
        property $
            \(a :: ControlMessage) -> (decode . encode) a `shouldBe` a
    it "TICK encodes as expected" $
        LBS.unpack (encode TICK) `shouldBe` [ 0, 0, 0, 0 ]
    it "LINK encodes as expected" $
        LBS.unpack (encode (LINK (pid "from" 1 2 3) (pid "to" 4 5 6))) `shouldBe`
            [ 0
            , 0
            , 0
            , 38
            , 112
            , 131
            , 104
            , 3
            , 97
            , 1
            , 103
            , 100
            , 0
            , 4
            , 102
            , 114
            , 111
            , 109
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 2
            , 3
            , 103
            , 100
            , 0
            , 2
            , 116
            , 111
            , 0
            , 0
            , 0
            , 4
            , 0
            , 0
            , 0
            , 5
            , 6
            ]
    it "SEND encodes as expected" $
        LBS.unpack (encode (SEND (pid "to" 4 5 6) (atom "hello"))) `shouldBe`
            [ 0
            , 0
            , 0
            , 33
            , 112
            , 131
            , 104
            , 3
            , 97
            , 2
            , 100
            , 0
            , 0
            , 103
            , 100
            , 0
            , 2
            , 116
            , 111
            , 0
            , 0
            , 0
            , 4
            , 0
            , 0
            , 0
            , 5
            , 6
            , 131
            , 100
            , 0
            , 5
            , 104
            , 101
            , 108
            , 108
            , 111
            ]
    it "EXIT encodes as expected" $
        LBS.unpack (encode (EXIT (pid "from" 1 2 3) (pid "to" 4 5 6) (atom "normal"))) `shouldBe`
            [ 0
            , 0
            , 0
            , 47
            , 112
            , 131
            , 104
            , 4
            , 97
            , 3
            , 103
            , 100
            , 0
            , 4
            , 102
            , 114
            , 111
            , 109
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 2
            , 3
            , 103
            , 100
            , 0
            , 2
            , 116
            , 111
            , 0
            , 0
            , 0
            , 4
            , 0
            , 0
            , 0
            , 5
            , 6
            , 100
            , 0
            , 6
            , 110
            , 111
            , 114
            , 109
            , 97
            , 108
            ]
    it "UNLINK encodes as expected" $
        LBS.unpack (encode (UNLINK (pid "from" 1 2 3) (pid "to" 4 5 6))) `shouldBe`
            [ 0
            , 0
            , 0
            , 38
            , 112
            , 131
            , 104
            , 3
            , 97
            , 4
            , 103
            , 100
            , 0
            , 4
            , 102
            , 114
            , 111
            , 109
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 2
            , 3
            , 103
            , 100
            , 0
            , 2
            , 116
            , 111
            , 0
            , 0
            , 0
            , 4
            , 0
            , 0
            , 0
            , 5
            , 6
            ]
    it "NODE_LINK encodes as expected" $
        LBS.unpack (encode NODE_LINK) `shouldBe` [ 0, 0, 0, 6, 112, 131, 104, 1, 97, 5 ]
    it "REG_SEND encodes as expected" $
        LBS.unpack (encode (REG_SEND (pid "from" 1 2 3) (atom "to") (atom "hello"))) `shouldBe`
            [ 0
            , 0
            , 0
            , 40
            , 112
            , 131
            , 104
            , 4
            , 97
            , 6
            , 103
            , 100
            , 0
            , 4
            , 102
            , 114
            , 111
            , 109
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 2
            , 3
            , 100
            , 0
            , 0
            , 100
            , 0
            , 2
            , 116
            , 111
            , 131
            , 100
            , 0
            , 5
            , 104
            , 101
            , 108
            , 108
            , 111
            ]
    it "GROUP_LEADER encodes as expected" $
        LBS.unpack (encode (GROUP_LEADER (pid "from" 1 2 3) (pid "to" 4 5 6))) `shouldBe`
            [ 0
            , 0
            , 0
            , 38
            , 112
            , 131
            , 104
            , 3
            , 97
            , 7
            , 103
            , 100
            , 0
            , 4
            , 102
            , 114
            , 111
            , 109
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 2
            , 3
            , 103
            , 100
            , 0
            , 2
            , 116
            , 111
            , 0
            , 0
            , 0
            , 4
            , 0
            , 0
            , 0
            , 5
            , 6
            ]
    it "EXIT2 encodes as expected" $
        LBS.unpack (encode (EXIT2 (pid "from" 1 2 3) (pid "to" 4 5 6) (atom "normal"))) `shouldBe`
            [ 0
            , 0
            , 0
            , 47
            , 112
            , 131
            , 104
            , 4
            , 97
            , 8
            , 103
            , 100
            , 0
            , 4
            , 102
            , 114
            , 111
            , 109
            , 0
            , 0
            , 0
            , 1
            , 0
            , 0
            , 0
            , 2
            , 3
            , 103
            , 100
            , 0
            , 2
            , 116
            , 111
            , 0
            , 0
            , 0
            , 4
            , 0
            , 0
            , 0
            , 5
            , 6
            , 100
            , 0
            , 6
            , 110
            , 111
            , 114
            , 109
            , 97
            , 108
            ]
