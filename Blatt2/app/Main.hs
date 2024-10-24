module Main where

import qualified Scanner

main :: IO ()
main = do
  print (Scanner.lexer "class public int 76")

 
