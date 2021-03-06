{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parser where

import Data.Maybe (fromMaybe)
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (comma))
import qualified Text.Parsec.Token as Tok

parseProgram :: String -> Either ParseError [Rule]
parseProgram = parse (spaces >> program) "<parser>"

parsePredicate :: String -> Either ParseError Predicate
parsePredicate = parse (spaces >> predicate) "<parser>"

program :: Parser [Rule]
program = many rule <* spaces

rule :: Parser Rule
rule = do
  head <- predicate
  maybeBody <- optionMaybe (string ":-" >> spaces >> commaSep1 predicate)
  char '.'
  spaces
  return $ Rule head (fromMaybe [] maybeBody)

predicate :: Parser Predicate
predicate = do
  nameHead <- lower
  nameTail <- fromMaybe [] <$> optionMaybe identifier
  terms <- parens $ commaSep term
  spaces
  return $ Predicate (nameHead : nameTail) terms

term :: Parser Term
term =
  do
    TermPredicate <$> try predicate
    <|> (TermLiteral <$> literal)
    <|> TermVariable <$> variable

variable :: Parser Variable
variable = do
  nameHead <- upper <|> char '_'
  nameTail <- fromMaybe [] <$> optionMaybe identifier
  spaces
  return $ Variable (nameHead : nameTail)

literal :: Parser Literal
literal = do
  literalAtom
    <|> literalInt

literalAtom :: Parser Literal
literalAtom = do
  nameHead <- lower
  nameTail <- fromMaybe [] <$> optionMaybe identifier
  spaces
  return $ LiteralAtom (nameHead : nameTail)

literalInt :: Parser Literal
literalInt = do
  n <- integer
  spaces
  return $ LiteralInt (fromInteger n)
