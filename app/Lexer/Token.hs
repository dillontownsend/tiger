module Lexer.Token (Token (..), AbsolutePosition) where

import qualified Data.Text as T (Text)

type AbsolutePosition = Int

data Token
  = -- symbols
    ASSIGN AbsolutePosition AbsolutePosition
  | OR AbsolutePosition AbsolutePosition
  | AND AbsolutePosition AbsolutePosition
  | GE AbsolutePosition AbsolutePosition
  | GT AbsolutePosition AbsolutePosition
  | LE AbsolutePosition AbsolutePosition
  | LT AbsolutePosition AbsolutePosition
  | NEQ AbsolutePosition AbsolutePosition
  | EQ AbsolutePosition AbsolutePosition
  | DIVIDE AbsolutePosition AbsolutePosition
  | TIMES AbsolutePosition AbsolutePosition
  | MINUS AbsolutePosition AbsolutePosition
  | PLUS AbsolutePosition AbsolutePosition
  | DOT AbsolutePosition AbsolutePosition
  | RBRACE AbsolutePosition AbsolutePosition
  | LBRACE AbsolutePosition AbsolutePosition
  | RBRACK AbsolutePosition AbsolutePosition
  | LBRACK AbsolutePosition AbsolutePosition
  | RPAREN AbsolutePosition AbsolutePosition
  | LPAREN AbsolutePosition AbsolutePosition
  | SEMICOLON AbsolutePosition AbsolutePosition
  | COLON AbsolutePosition AbsolutePosition
  | COMMA AbsolutePosition AbsolutePosition
  | -- keywords
    TYPE AbsolutePosition AbsolutePosition
  | VAR AbsolutePosition AbsolutePosition
  | FUNCTION AbsolutePosition AbsolutePosition
  | BREAK AbsolutePosition AbsolutePosition
  | OF AbsolutePosition AbsolutePosition
  | END AbsolutePosition AbsolutePosition
  | IN AbsolutePosition AbsolutePosition
  | NIL AbsolutePosition AbsolutePosition
  | LET AbsolutePosition AbsolutePosition
  | DO AbsolutePosition AbsolutePosition
  | TO AbsolutePosition AbsolutePosition
  | FOR AbsolutePosition AbsolutePosition
  | WHILE AbsolutePosition AbsolutePosition
  | ELSE AbsolutePosition AbsolutePosition
  | THEN AbsolutePosition AbsolutePosition
  | IF AbsolutePosition AbsolutePosition
  | ARRAY AbsolutePosition AbsolutePosition
  | -- dynamic tokens
    STRING T.Text AbsolutePosition AbsolutePosition
  | INT Int AbsolutePosition AbsolutePosition
  | ID T.Text AbsolutePosition AbsolutePosition
  | EOF AbsolutePosition AbsolutePosition
  deriving (Eq, Show)
