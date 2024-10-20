module Main where

import Compilerbau

main :: IO ()
main = do
    putStrLn "Testing logical operators:"
    print $ halfAdder (True, False)       -- Should print (True, False)
    print $ fullAdder (True, True, False) -- Should print (False, True)
    putStrLn "Testing tree mapping:"
    let tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
    print $ mapTree (tree, show)           -- Should print Node "1" (Node "2" Empty Empty) (Node "3" Empty Empty)
    putStrLn "Testing toListofLists:"
    print $ toListofLists [1, 2, 3]        -- Should print [[1],[2],[3]]
    putStrLn "Testing listStringConcat:"
    print $ listStringConcat ["Hello, ", "World!"] -- Should print "Hello, World!"
