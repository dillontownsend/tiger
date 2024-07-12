module Main where

import Lexer.Lexer (lexInput)

main :: IO ()
main = do
  print $ lexInput "= | & - + . ] [ } { ) ( ; , : := > >= < <> <= 100 dillon"
