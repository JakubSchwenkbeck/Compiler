module Main where

import qualified Scanner

main :: IO ()
main = do
  putStrLn "\n"
  let string1 = "public static void main { }"
  putStrLn "The first input :" 
  print string1

  print $ Scanner.lexer string1


  let string2 = "private int VariableName = 200"
  putStrLn "\n The second input :"
  print string2
  print $ Scanner.lexer string2

 
  let string3 = "public static void main { public boolean b = true    private int Var = 1337 b + Var return 5}"
  putStrLn "\n The third input : "
  print string3
  print $ Scanner.lexer string3

  let string4 = "This is an error Test $$ §§ ``"
  putStrLn "\n This is an test for error / unexpected behavior"
  print string4
  print $ Scanner.lexer string4
 
 
