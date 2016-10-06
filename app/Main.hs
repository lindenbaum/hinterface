-- M-x intero-targets
-- hinterface:lib hinterface:hinterface-test
--
module Main ( main ) where

import           Language.Erlang.Epmd
import           Language.Erlang.LocalNode
import           Language.Erlang.Mailbox
import           Language.Erlang.Term
import           Person
import           Prelude                   hiding (length)

main :: IO ()
main = do
    epmdNames "localhost.localdomain" >>= print

    localNode <- newLocalNode "hay@localhost.localdomain" "cookie" >>= registerLocalNode

    epmdNames "localhost.localdomain" >>= print

    mailbox <- make_mailbox localNode
    let self = getPid mailbox

    myPort <- make_port localNode
    myRef <- make_ref localNode
    let message = ( self
                  , myRef
                  , myPort
                  , (float 2.18, (), list [], list [ atom "a", atom "b", atom "c" ])
                  , string "hello!"
                  )
    putStr "Message: "
    print message
    putStrLn ""

    sendReg mailbox "echo" "erl@localhost.localdomain" (toTerm message)


    reply <- receive mailbox
    putStr "Reply: "
    print reply
    putStrLn ""

    sendReg mailbox "echo" "erl@localhost.localdomain" (toTerm (self, Person "Timo" 46))
    person <- receive mailbox
    print person
    case fromTerm person :: Maybe Person of
        Just p -> print p
        Nothing -> putStrLn "NOPE!"

    putStrLn "BYE"

    register localNode "hay" self
    loop mailbox

    closeLocalNode localNode

    epmdNames "localhost.localdomain" >>= print

loop :: Mailbox -> IO ()
loop mailbox = do
    msg <- receive mailbox
    case fromTerm msg of
        Just (remotePid, i) ->
          do send mailbox remotePid (toTerm (integer (i + 1)))
             loop mailbox
        _ -> do
            return ()
