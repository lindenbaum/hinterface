-- M-x intero-targets
-- hinterface:lib hinterface:hinterface-test
--
module Main ( main ) where

import           Prelude                   hiding ( length )

import           Data.IOx

import           Language.Erlang.Epmd
import           Language.Erlang.LocalNode
import           Language.Erlang.Term
import           Language.Erlang.Mailbox

import           Person

main :: IO ()
main = fromIOx $ do
    mainX

mainX :: IOx ()
mainX = do
    epmdNames "localhost.localdomain" >>= printX

    localNode <- newLocalNode "hay@localhost.localdomain" "cookie" >>= registerLocalNode

    epmdNames "localhost.localdomain" >>= printX

    mailbox <- make_mailbox localNode
    let self = getPid mailbox

    myPort <- make_port localNode
    myRef <- make_ref localNode
    let message = tuple [ self
                        , myRef
                        , myPort
                        , tuple [ float 2.18, tuple [], list [], list [ atom "a", atom "b", atom "c" ] ]
                        , string "hello!"
                        ]
    liftIOx $ putStr "Message: "
    liftIOx $ print message
    liftIOx $ putStrLn ""

    sendReg mailbox "echo" "erl@localhost.localdomain" message


    reply <- receive mailbox
    liftIOx $ putStr "Reply: "
    liftIOx $ print reply
    liftIOx $ putStrLn ""

    sendReg mailbox "echo" "erl@localhost.localdomain" (tuple [ self, (toTerm (Person "Timo" 46)) ])
    person <- receive mailbox
    liftIOx $ print person
    case fromTerm person :: Maybe Person of
        Just p -> liftIOx $ print p
        Nothing -> liftIOx $ putStrLn "NOPE!"

    liftIOx $ putStrLn "BYE"

    register localNode "hay" self
    loopX mailbox

    closeLocalNode localNode

    epmdNames "localhost.localdomain" >>= printX

loopX :: Mailbox -> IOx ()
loopX mailbox = do
    msg <- receive mailbox
    case match_tuple msg of
        Just [ remotePid, value ] -> do
            case to_integer value of
                Just i -> do
                    send mailbox remotePid (integer (i + 1))
                    loopX mailbox
                _ -> do
                    return ()
        _ -> do
            return ()
