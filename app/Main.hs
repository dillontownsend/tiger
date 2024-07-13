module Main where

import Lexer.Lexer (lexInput)

main :: IO ()
main = do
  input <- readFile "app/test.tig"
  putStrLn input
  print input
  print $ lexInput input

-- input :: String
-- input = "= | & - + . ] [ } { ) ( ; , : := > >= < <> <= 100 dillon"
