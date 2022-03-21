module Syntax where

data Rule = Rule Predicate [Predicate] deriving (Show, Eq)

data Predicate = Predicate String [Term] deriving (Show, Eq)

data Term
  = TermPredicate Predicate
  | TermVariable Variable
  | TermLiteral Literal
  deriving (Show, Eq)

newtype Variable = Variable String deriving (Show, Eq)

data Literal
  = LiteralAtom String
  | LiteralInt Int
  deriving (Show, Eq)
