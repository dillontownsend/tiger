{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Lexer.Lexer (runLexer) where

import Control.Applicative (liftA2)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State (State, evalState, get, gets, modify)
import Data.Char (isAlpha, isDigit)
import qualified Data.Text as T (Text, cons, tail, uncons, unpack)
import Lexer.Token (AbsolutePosition, Token (..))
import Prelude hiding (EQ, GT, LT)

type LineNumber = Int

type ColumnNumber = Int

type LocalLength = Int

data LexerState = LexerState
  { input :: T.Text,
    lineNumber :: LineNumber,
    columnNumber :: ColumnNumber,
    absolutePosition :: AbsolutePosition,
    localLength :: LocalLength
  }

data LexicalError
  = IllegalToken Char LineNumber ColumnNumber
  | UnclosedString LineNumber ColumnNumber
  | UnclosedComment LineNumber ColumnNumber

lexicalErrorPrefix :: LineNumber -> ColumnNumber -> String
lexicalErrorPrefix line column = "Lexical error at line " ++ show line ++ " column " ++ show column ++ ": "

instance Show LexicalError where
  show (IllegalToken c line column) = lexicalErrorPrefix line column ++ "illegal token '" ++ [c] ++ "'"
  show (UnclosedString line column) = lexicalErrorPrefix line column ++ "unclosed string"
  show (UnclosedComment line column) = lexicalErrorPrefix line column ++ "unclosed comment"

type Lexer = ExceptT LexicalError (State LexerState)

pattern (:>) :: Char -> T.Text -> T.Text
pattern c :> t <- (T.uncons -> Just (c, t))

pattern Empty :: T.Text
pattern Empty <- (T.uncons -> Nothing)

readT :: (Read a) => T.Text -> a
readT = read . T.unpack

nextToken :: Lexer Token
nextToken = do
  skipWhitespace
  inp <- gets input
  case inp of
    Empty -> makeToken EOF
    '=' :> _ -> advance1 >> makeToken EQ
    '|' :> _ -> advance1 >> makeToken OR
    '&' :> _ -> advance1 >> makeToken AND
    '/' :> ('*' :> _) -> advanceN 2 >> skipComment >> nextToken
    '/' :> _ -> advance1 >> makeToken DIVIDE
    '*' :> _ -> advance1 >> makeToken TIMES
    '-' :> _ -> advance1 >> makeToken MINUS
    '+' :> _ -> advance1 >> makeToken PLUS
    '.' :> _ -> advance1 >> makeToken DOT
    ']' :> _ -> advance1 >> makeToken RBRACE
    '[' :> _ -> advance1 >> makeToken LBRACE
    '}' :> _ -> advance1 >> makeToken RBRACK
    '{' :> _ -> advance1 >> makeToken LBRACK
    ')' :> _ -> advance1 >> makeToken RPAREN
    '(' :> _ -> advance1 >> makeToken LPAREN
    ';' :> _ -> advance1 >> makeToken SEMICOLON
    ',' :> _ -> advance1 >> makeToken COMMA
    '"' :> _ -> advance1 >> lexString >>= makeToken . STRING
    ':' :> ('=' :> _) -> advanceN 2 >> makeToken ASSIGN
    ':' :> _ -> advance1 >> makeToken COLON
    '>' :> ('=' :> _) -> advanceN 2 >> makeToken GE
    '>' :> _ -> advance1 >> makeToken GT
    '<' :> ('>' :> _) -> advanceN 2 >> makeToken NEQ
    '<' :> ('=' :> _) -> advanceN 2 >> makeToken LE
    '<' :> _ -> advance1 >> makeToken LT
    c :> _
      | isDigit c -> advanceWhile isDigit >>= makeToken . INT . readT
      | isAlpha c -> lexWord
    illegalToken :> _ -> throwLexicalError $ IllegalToken illegalToken

lexWord :: Lexer Token
lexWord = do
  word <- advanceWhile isLetter
  makeToken
    ( case word of
        "type" -> TYPE
        "var" -> VAR
        "function" -> FUNCTION
        "break" -> BREAK
        "of" -> OF
        "end" -> END
        "in" -> IN
        "nil" -> NIL
        "let" -> LET
        "do" -> DO
        "to" -> TO
        "for" -> FOR
        "while" -> WHILE
        "else" -> ELSE
        "then" -> THEN
        "if" -> IF
        "array" -> ARRAY
        _ -> ID word
    )

lexString :: Lexer T.Text
lexString = do
  inp <- gets input
  case inp of
    Empty -> throwLexicalError UnclosedString
    '\\' :> ('n' :> _) -> recurseEscapeChar '\n'
    '\\' :> ('t' :> _) -> recurseEscapeChar '\t'
    '\\' :> ('"' :> _) -> recurseEscapeChar '\"'
    '\\' :> ('\\' :> _) -> recurseEscapeChar '\\'
    '"' :> _ -> advance1 >> pure ""
    c :> _ -> T.cons c <$> (advance1 >> lexString)
  where
    recurseEscapeChar :: Char -> Lexer T.Text
    recurseEscapeChar = flip fmap (advanceN 2 >> lexString) . T.cons

advanceWhile :: (Char -> Bool) -> Lexer T.Text
advanceWhile predicate = do
  inp <- gets input
  case inp of
    Empty -> pure ""
    c :> _
      | not $ predicate c -> pure ""
      | otherwise -> T.cons c <$> (advance1 >> advanceWhile predicate)

isLetter :: Char -> Bool
isLetter '_' = True
isLetter c
  | isAlpha c || isDigit c = True
  | otherwise = False

skipComment :: Lexer ()
skipComment = do
  inp <- gets input
  case inp of
    Empty -> throwLexicalError UnclosedComment
    '*' :> ('/' :> _) -> advanceN 2 >> resetLocalLength
    _ -> advance1 >> skipComment

skipWhitespace :: Lexer ()
skipWhitespace = do
  inp <- gets input
  case inp of
    Empty -> resetLocalLength
    ' ' :> _ -> skip
    '\n' :> _ -> skip
    '\t' :> _ -> skip
    _ -> resetLocalLength
  where
    skip = advance1 >> skipWhitespace

resetLocalLength :: Lexer ()
resetLocalLength = modify (\lexerState -> lexerState {localLength = 0})

advance1 :: Lexer ()
advance1 = do
  modify
    ( \lexerState@LexerState
         { input = inp,
           lineNumber = ln,
           columnNumber = cn,
           absolutePosition = ap,
           localLength = ll
         } ->
          let advancedInput = T.tail inp
              advancedAbsolutePosition = ap + 1
              advancedLocalLength = ll + 1
           in case inp of
                Empty -> lexerState
                '\n' :> _ ->
                  lexerState
                    { input = advancedInput,
                      lineNumber = ln + 1,
                      columnNumber = 1,
                      absolutePosition = advancedAbsolutePosition,
                      localLength = advancedLocalLength
                    }
                _ ->
                  lexerState
                    { input = advancedInput,
                      columnNumber = cn + 1,
                      absolutePosition = advancedAbsolutePosition,
                      localLength = advancedLocalLength
                    }
    )

advanceN :: Int -> Lexer ()
advanceN 0 = pure ()
advanceN n
  | n < 0 = error "n must be >= 0, silly goose"
  | otherwise = advance1 >> advanceN (n - 1)

accumulateTokens :: Lexer [Token]
accumulateTokens = do
  token <- nextToken
  case token of
    EOF _ _ -> pure [token]
    _ -> (token :) <$> accumulateTokens

initLexerState :: T.Text -> LexerState
initLexerState inp =
  LexerState
    { input = inp,
      lineNumber = 1,
      columnNumber = 1,
      absolutePosition = 1,
      localLength = 0
    }

throwLexicalError :: (LineNumber -> ColumnNumber -> LexicalError) -> Lexer a
throwLexicalError constructor = liftA2 constructor (gets lineNumber) (gets columnNumber) >>= throwError

makeToken :: (AbsolutePosition -> AbsolutePosition -> Token) -> Lexer Token
makeToken constructor = do
  LexerState {absolutePosition = ap, localLength = ll} <- get
  resetLocalLength
  pure $ constructor (ap - ll) ap

runLexer :: T.Text -> Either LexicalError [Token]
runLexer = evalState (runExceptT accumulateTokens) . initLexerState
