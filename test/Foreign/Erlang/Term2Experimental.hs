{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors -Wno-missing-pattern-synonym-signatures #-}

module Foreign.Erlang.Term2Experimental where

import Data.Proxy
import Data.String
import qualified Data.Vector as V
import qualified Foreign.Erlang.Term as SomeTerm
import GHC.TypeLits

data Terms
  = Atoms
  | AtomSymbols Symbol
  | Integers
  | Nils
  | Tupels [Terms]
  | Lists Terms Terms

data Term (t :: Terms) where
  Atom :: String -> Term 'Atoms
  AtomSymbol :: forall a. KnownSymbol a => Term ('AtomSymbols a)
  Integer :: Integer -> Term 'Integers
  Nil :: Term 'Nils
  Unit :: Term ('Tupels '[])
  ConsTuple :: Term t -> Term ('Tupels ts) -> Term ('Tupels (t ': ts))
  List :: [Term e] -> Term t -> Term ('Lists e t)

pattern Tup1 t1 = ConsTuple t1 Unit

pattern Tup2 t1 t2 = ConsTuple t1 (Tup1 t2)

pattern Tup3 t1 t2 t3 = ConsTuple t1 (Tup2 t2 t3)

pattern Tup4 t1 t2 t3 t4 = ConsTuple t1 (Tup3 t2 t3 t4)

pattern Tup5 t1 t2 t3 t4 t5 = ConsTuple t1 (Tup4 t2 t3 t4 t5)

pattern Tup6 t1 t2 t3 t4 t5 t6 = ConsTuple t1 (Tup5 t2 t3 t4 t5 t6)

pattern Tup7 t1 t2 t3 t4 t5 t6 t7 = ConsTuple t1 (Tup6 t2 t3 t4 t5 t6 t7)

-- instance SomeTerm.FromTerm (Term Atoms) where
--   fromTerm a@(SomeTerm.Atom _) = Just (Atom (show a))
--   fromTerm _ = Nothing

instance SomeTerm.ToTerm (Term t) where
  toTerm (Atom a) = SomeTerm.atom (fromString a)
  toTerm a@AtomSymbol =
    let px :: Term (AtomSymbols s) -> Proxy s
        px _ = Proxy
     in SomeTerm.atom (fromString (symbolVal (px a)))
  toTerm (Integer i) = SomeTerm.integer i
  toTerm Nil = SomeTerm.Nil
  toTerm (List xs t) = SomeTerm.List (V.fromList (fmap SomeTerm.toTerm xs)) (SomeTerm.toTerm t)
  toTerm Unit = SomeTerm.tuple []
  toTerm xs@(ConsTuple _ _) = SomeTerm.tuple (go xs)
    where
      go :: Term (Tupels x_ys) -> [SomeTerm.Term]
      go (ConsTuple x ys) = SomeTerm.toTerm x : go ys
      go Unit = []

type EventTerm t = Term ('Tupels '[ 'AtomSymbols "Event", t])

pattern Event :: Term t -> EventTerm t
pattern Event xx = Tup2 AtomSymbol xx

type BlubDown = 'Tupels [AtomSymbols "BlubDown", Integers, Atoms]

pattern BlubEvent :: Term 'Integers -> EventTerm BlubDown
pattern BlubEvent blubIndex <- Event (Tup3 AtomSymbol blubIndex _)
