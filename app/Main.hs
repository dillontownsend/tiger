module Main where

import Lexer.Lexer (runLexer)

main :: IO ()
main = do
  input <- readFile "app/test.tig"
  putStrLn input
  print $ runLexer input
