{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Text.IO as TIO (putStrLn, readFile)
import Lexer.Lexer (runLexer)
import System.CPUTime (getCPUTime)

main :: IO ()
main = do
  start <- getCPUTime
  input <- TIO.readFile "app/test.tig"
  let !lexemes = runLexer input
  end <- getCPUTime
  print (end - start)
