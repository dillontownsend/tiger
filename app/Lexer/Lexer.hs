module Lexer.Lexer (lexInput) where

import Control.Applicative (liftA2)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State (State, evalState, get, gets, put)
import Data.Char (isAlpha, isDigit)
import Lexer.Token (Token (..))
import Prelude hiding (EQ, GT, LT)

type Input = String

type LineNumber = Int

type ColumnNumber = Int

data LexerState = LexerState
  { input :: Input,
    lineNumber :: LineNumber,
    columnNumber :: ColumnNumber
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

-- TODO: line numbers in tokens
-- TODO: clean up
-- TODO: Data.Text

nextToken :: Lexer Token
nextToken = do
  skipWhitespace
  inp <- gets input
  case inp of
    "" -> pure EOF
    ('=' : _) -> advance1 >> pure EQ
    ('|' : _) -> advance1 >> pure OR
    ('&' : _) -> advance1 >> pure AND
    ('/' : '*' : _) -> advanceN 2 >> skipComment >> nextToken
    ('/' : _) -> advance1 >> pure DIVIDE
    ('*' : _) -> advance1 >> pure TIMES
    ('-' : _) -> advance1 >> pure MINUS
    ('+' : _) -> advance1 >> pure PLUS
    ('.' : _) -> advance1 >> pure DOT
    (']' : _) -> advance1 >> pure RBRACE
    ('[' : _) -> advance1 >> pure LBRACE
    ('}' : _) -> advance1 >> pure RBRACK
    ('{' : _) -> advance1 >> pure LBRACK
    (')' : _) -> advance1 >> pure RPAREN
    ('(' : _) -> advance1 >> pure LPAREN
    (';' : _) -> advance1 >> pure SEMICOLON
    (',' : _) -> advance1 >> pure COMMA
    ('"' : _) -> advance1 >> lexString >>= pure . STRING
    (':' : '=' : _) -> advanceN 2 >> pure ASSIGN
    (':' : _) -> advance1 >> pure COLON
    ('>' : '=' : _) -> advanceN 2 >> pure GE
    ('>' : _) -> advance1 >> pure GT
    ('<' : '>' : _) -> advanceN 2 >> pure NEQ
    ('<' : '=' : _) -> advanceN 2 >> pure LE
    ('<' : _) -> advance1 >> pure LT
    (c : _)
      | isDigit c -> INT . read <$> advanceWhile isDigit
      | isAlpha c -> lexWord
    (illegalToken : _) -> throwLexicalError $ IllegalToken illegalToken

lexWord :: Lexer Token
lexWord = do
  word <- advanceWhile isLetter
  pure
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

lexString :: Lexer String
lexString = do
  inp <- gets input
  case inp of
    [] -> throwLexicalError UnclosedString
    ('\\' : 'n' : _) -> recurseEscapeChar '\n'
    ('\\' : 't' : _) -> recurseEscapeChar '\t'
    ('\\' : '"' : _) -> recurseEscapeChar '\"'
    ('\\' : '\\' : _) -> recurseEscapeChar '\\'
    ('"' : _) -> advance1 >> pure ""
    _ -> (head inp :) <$> (advance1 >> lexString)
  where
    recurseEscapeChar :: Char -> Lexer String
    recurseEscapeChar = flip fmap (advanceN 2 >> lexString) . (:)

advanceWhile :: (Char -> Bool) -> Lexer String
advanceWhile predicate = do
  inp <- gets input
  case inp of
    "" -> pure ""
    (c : _)
      | not $ predicate c -> pure ""
      | otherwise -> (c :) <$> (advance1 >> advanceWhile predicate)

isLetter :: Char -> Bool
isLetter '_' = True
isLetter c
  | isAlpha c || isDigit c = True
  | otherwise = False

skipComment :: Lexer ()
skipComment = do
  inp <- gets input
  case inp of
    [] -> throwLexicalError UnclosedComment
    ('*' : '/' : _) -> advanceN 2
    _ -> advance1 >> skipComment

skipWhitespace :: Lexer ()
skipWhitespace = do
  inp <- gets input
  case inp of
    [] -> pure ()
    (' ' : _) -> skip
    ('\n' : _) -> skip
    ('\t' : _) -> skip
    _ -> pure ()
  where
    skip = advance1 >> skipWhitespace

advance1 :: Lexer ()
advance1 = do
  LexerState {input = inp, lineNumber = line, columnNumber = column} <- get
  let advancedInput = tail inp
  case inp of
    "" -> pure ()
    ('\n' : _) -> put LexerState {input = advancedInput, lineNumber = line + 1, columnNumber = 1}
    _ -> put LexerState {input = advancedInput, lineNumber = line, columnNumber = column + 1}

advanceN :: Int -> Lexer ()
advanceN 0 = pure ()
advanceN n
  | n < 0 = error "n must be >= 0, silly goose"
  | otherwise = advance1 >> advanceN (n - 1)

accumulateTokens :: Lexer [Token]
accumulateTokens = do
  token <- nextToken
  if token == EOF
    then pure [token]
    else (token :) <$> accumulateTokens

initLexerState :: Input -> LexerState
initLexerState inp =
  LexerState
    { input = inp,
      lineNumber = 1,
      columnNumber = 1
    }

throwLexicalError :: (LineNumber -> ColumnNumber -> LexicalError) -> Lexer a
throwLexicalError constructor = liftA2 constructor (gets lineNumber) (gets columnNumber) >>= throwError

lexInput :: Input -> Either LexicalError [Token]
lexInput = evalState (runExceptT accumulateTokens) . initLexerState
