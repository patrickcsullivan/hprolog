module Lexer where

import Text.Parsec (char)
import qualified Text.Parsec as Tok
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (symbol))
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = [],
          Tok.reservedNames = []
        }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
