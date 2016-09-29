module Data.IOxSpec ( spec ) where

import           Test.Hspec

import           System.IO.Unsafe
import           Data.IORef

import           Data.IOx
import           Control.Monad

spec :: Spec
spec = do
    describe "toIOx" $ do
        it "converts IO errors" $
            (runIOx $ (toIOx $ ioError (userError "ioError")) `catchX` (return . show)) `shouldBe` "user error (ioError)"
    describe "throwX" $ do
        it "throws an error" $
            (runIOx $ errorX userErrorType "rethrown" `catchX` throwX `catchX` (return . show)) `shouldBe`
                "rethrown: user error"
    describe "catchX" $ do
        it "does not run the handler for a succesful action" $
            (runIOx $ action1 `catchX` undefined) `shouldBe` 1
        it "runs the handler for a failed action" $
            (runIOx $ actionX `catchX` handler2) `shouldBe` 2
        it "runs the outer handler for a failed inner handler" $
            (runIOx $ actionX `catchX` handlerX `catchX` handler2) `shouldBe` 2
    describe "foreverX" $ do
        it "may throw an error immediately" $
            (runIOx $ foreverX actionX `catchX` handler2) `shouldBe` 2
        it "may throw an error after some iterations" $ do
            (runIOx $ do
                 counter <- liftIOx $ newIORef (0 :: Int)
                 (foreverX $ do
                      val' <- liftIOx $ atomicModifyIORef' counter (\val -> (val + 1, val + 1))
                      when (val' >= 42) $
                          errorX alreadyExistsErrorType (show val')) `catchX`
                     (return . show)) `shouldBe`
                "42: already exists"

action1 :: IOx Int
action1 = do
    return 1

actionX :: IOx Int
actionX = do
    errorX userErrorType "actionX"

handler2 :: IOError -> IOx Int
handler2 _ = do
    return 2

handlerX :: IOError -> IOx Int
handlerX _ = do
    errorX userErrorType "handlerX"

runIOx :: IOx a -> a
runIOx = unsafePerformIO . fromIOx
