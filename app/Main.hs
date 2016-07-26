module Main ( main
            )
       where

import Prelude hiding (length)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import Data.Word
import Data.Char

import Util.IOx
import Language.Erlang.Epmd
import Language.Erlang.LocalNode
import Language.Erlang.Term
import Language.Erlang.Mailbox

--------------------------------------------------------------------------------

printEpmdNamesResponse :: (Word32, BS.ByteString) -> IOx ()
printEpmdNamesResponse (epmdPortNo, nodeInfos) = toIOx $ do
  putStr "Epmd Port: " ; print epmdPortNo
  mapM_ putStrLn (lines (map (chr . fromIntegral) (BS.unpack nodeInfos)))

--------------------------------------------------------------------------------

main :: IO ()
main = fromIOx $ do
  mainX

mainX :: IOx ()
mainX = do
  epmdNames "localhost.localdomain" >>= printX
  liftIOx $ putStrLn ""

  localNode <- newLocalNode "hay@localhost.localdomain" "cookie"

  mailbox <- make_mailbox localNode
  let self = getPid mailbox

  myPort <- make_port localNode
  myRef <- make_ref localNode
  let message = tuple [self, myRef, myPort, tuple [float 2.18, tuple [], list [], list [atom "a", atom "b", atom "c"]], string "hello!"]
  liftIOx $ putStr "Message: "
  liftIOx $ print message
  liftIOx $ putStrLn ""

  sendReg mailbox "echo" "erl@localhost.localdomain" message
  reply <- receive mailbox
  liftIOx $ putStr "Reply: "
  liftIOx $ print reply
  liftIOx $ putStrLn ""

  liftIOx $ putStrLn "BYE"

  closeLocalNode localNode

data Person = Person String Int

instance ToTerm Person where
  toTerm (Person name age) = tuple [string (CS.pack name), integer (fromIntegral age)]

instance FromTerm Person where
  fromTerm term
    | is_tuple term &&
      length term == 2 &&
      (is_list $ element 1 term) &&
      (is_integer $ element 2 term) = undefined
    | otherwise                 = Nothing
