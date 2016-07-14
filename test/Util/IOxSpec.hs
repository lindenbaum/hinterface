module Util.IOxSpec ( spec
                    )
       where

import Test.Hspec

import System.IO.Unsafe

import Util.IOx

spec :: Spec
spec = do
  describe "catchX" $ do
    it "does not run the handler for a succesful action" $
      (runIOx $ action1 `catchX` undefined) `shouldBe` 1
    it "runs the handler for a failed action" $
      (runIOx $ actionX `catchX` handler2) `shouldBe` 2
    it "runs the outer handler for a failed inner handler" $
      (runIOx $ actionX `catchX` handlerX `catchX` handler2) `shouldBe` 2


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
