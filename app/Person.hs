module Person where

import qualified Data.ByteString.Char8 as CS

import           Language.Erlang.Term

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
