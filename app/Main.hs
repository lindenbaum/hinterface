module Main ( main ) where

import           Prelude                   hiding ( length )

import qualified Data.ByteString.Char8     as CS

import           Util.IOx

import           Language.Erlang.Epmd
import           Language.Erlang.LocalNode
import           Language.Erlang.Term
import           Language.Erlang.Mailbox

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
    msg <- receive mailbox

    liftIOx $ putStr "Msg: "
    liftIOx $ print msg
    liftIOx $ putStrLn ""

    closeLocalNode localNode

    epmdNames "localhost.localdomain" >>= printX

data Person = Person String Integer
    deriving (Eq, Show)

instance ToTerm Person where
    toTerm (Person name age) =
        tuple [ (atom "person"), string (CS.pack name), integer (fromIntegral age) ]

instance FromTerm Person where
    fromTerm term = case match_tuple term of
        Just [ tag, name, age ] ->
            match_atom tag "person" >> Person <$> (CS.unpack <$> to_string name) <*> (to_integer age)
        _ -> Nothing
