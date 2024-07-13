module Main where

import Lexer.Lexer (lexInput)

main :: IO ()
main = do
  input <- readFile "app/test.tig"
  putStrLn input
  print $ lexInput input
