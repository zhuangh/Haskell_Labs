module Main where

import qualified Hw2 as H

numList :: [Int]
numList = [1..9]

zero :: Int
zero = 0

myBST :: H.BST Int Char 
myBST = H.Bind 10 'f' (H.Bind 5 'w' (H.Bind 2 'a' (H.Bind 1 'b' H.Emp H.Emp) (H.Bind 3 'c' H.Emp H.Emp) ) (H.Bind 8 'e' (H.Bind 6 'f' H.Emp H.Emp ) (H.Bind 9 'g' H.Emp H.Emp) ) ) (H.Bind 12 'o' H.Emp H.Emp) 

main :: IO ()
main = do putStrLn (show (H.myFoldl (\l x -> x : l) [] numList))
          putStrLn (show (H.myReverse numList))
          putStrLn (show (H.myFoldr (\_ n -> 1 + n) zero numList))
          putStrLn (show (H.myFoldr (\x xs -> (x, x + 1) : xs) [] numList))
          putStrLn (show (H.myFoldl2 (\l x -> x : l) [] numList))
          putStrLn (show myBST)
          putStrLn (show (H.delete 5 myBST))
          putStrLn (show (H.delete 10 myBST))
          putStrLn (show (H.delete 10 (H.delete 5 myBST)))
          putStrLn (show (H.delete 12 myBST))
          putStrLn (show (H.delete 2 myBST))
          H.main

