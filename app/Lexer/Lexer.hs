module Lexer.Lexer (lexInput) where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State (evalState, gets, modify)
import Control.Monad.State.Lazy (State)
import Data.Char (isAlpha, isDigit)
import Lexer.Token (Token (..))
import Prelude hiding (EQ, GT, LT)

type Input = String

data LexerState = LexerState
  { input :: Input,
    stateCode :: Int
  }

data LexicalError
  = IllegalToken Char
  | UnclosedString
  | UnclosedComment

lexicalError :: String
lexicalError = "Lexical error: "

instance Show LexicalError where
  show (IllegalToken c) = lexicalError ++ "illegal token '" ++ show c ++ "'"
  show UnclosedString = lexicalError ++ "unclosed string"
  show UnclosedComment = lexicalError ++ "unclosed comment"

type Lexer = ExceptT LexicalError (State LexerState)

nextToken :: Lexer Token
nextToken = do
  skipWhitespace
  inp <- gets input
  if null inp
    then pure EOF
    else case head inp of
      '=' -> advanceWithToken EQ
      '|' -> advanceWithToken OR
      '&' -> advanceWithToken AND
      '/' ->
        advance >> do
          advancedInput <- gets input
          case advancedInput of
            [] -> pure TIMES
            _
              | head advancedInput == '*' ->
                  advance >> skipComment >> nextToken
              | otherwise -> pure TIMES
      '*' -> advanceWithToken TIMES
      '-' -> advanceWithToken MINUS
      '+' -> advanceWithToken PLUS
      '.' -> advanceWithToken DOT
      ']' -> advanceWithToken RBRACE
      '[' -> advanceWithToken LBRACE
      '}' -> advanceWithToken RBRACK
      '{' -> advanceWithToken LBRACK
      ')' -> advanceWithToken RPAREN
      '(' -> advanceWithToken LPAREN
      ';' -> advanceWithToken SEMICOLON
      ',' -> advanceWithToken COMMA
      '"' -> advance >> lexString >>= pure . STRING
      ':' ->
        advance >> do
          isExpected <- expectChar '='
          if isExpected
            then advanceWithToken ASSIGN
            else pure COLON
      '>' ->
        advance >> do
          isExpected <- expectChar '='
          if isExpected
            then advanceWithToken GE
            else pure GT
      '<' ->
        advance >> do
          advancedInput <- gets input
          case advancedInput of
            "" -> pure LT
            _
              | c == '>' -> advanceWithToken NEQ
              | c == '=' -> advanceWithToken LE
              | otherwise -> pure LT
              where
                c = head advancedInput
      c
        | isDigit c -> INT . read <$> seekWhile isDigit
        | isAlpha c -> lexWord
      illegalToken -> throwError $ IllegalToken illegalToken

lexWord :: Lexer Token
lexWord = do
  word <- seekWhile isLetter
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
    [] -> throwError UnclosedString
    _
      | c == '\\' -> do
          advance
          isQuote <- expectChar '"'
          if isQuote
            then ('\"' :) <$> (advance >> lexString)
            else (c :) <$> lexString
      | c == '"' -> advance >> pure ""
      | otherwise -> (c :) <$> (advance >> lexString)
      where
        c = head inp

seekWhile :: (Char -> Bool) -> Lexer String
seekWhile predicate = do
  inp <- gets input
  let c = head inp
  if null inp || not (predicate c)
    then pure ""
    else (c :) <$> (advance >> seekWhile predicate)

isLetter :: Char -> Bool
isLetter c
  | isAlpha c || c == '_' = True
  | otherwise = False

advanceWithToken :: Token -> Lexer Token
advanceWithToken = (advance >>) . pure

skipComment :: Lexer ()
skipComment = do
  inp <- gets input
  case inp of
    [] -> throwError UnclosedComment
    ('*' : '/' : _) -> advance >> advance
    _ -> advance >> skipComment

skipWhitespace :: Lexer ()
skipWhitespace = do
  inp <- gets input
  if null inp
    then pure ()
    else case head inp of
      c
        | c == ' ' || c == '\n' || c == '\t' ->
            advance >> skipWhitespace
      _ -> pure ()

advance :: Lexer ()
advance =
  modify
    ( \lexerState ->
        let inp = input lexerState
         in if null inp
              then lexerState
              else lexerState {input = tail inp}
    )

expectChar :: Char -> Lexer Bool
expectChar c = do
  inp <- gets input
  case inp of
    [] -> pure False
    (c' : _)
      | c == c' -> pure True
      | otherwise -> pure False

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
      stateCode = 0
    }

lexInput :: Input -> Either LexicalError [Token]
lexInput = evalState (runExceptT accumulateTokens) . initLexerState