module Main where

import qualified Hw1 as H

myList :: [Int]
myList = [1..7]

myTree :: H.Tree [Char]
myTree = H.Branch (H.Branch (H.Branch (H.Leaf "a")
                                      (H.Leaf "b"))
                            (H.Branch (H.Leaf "c")
                                      (H.Leaf "d")))
                  (H.Branch           (H.Leaf "e")
                                      (H.Leaf "f"))

--      myTree
--        / \
--       /   \
--      .     \
--     / \     \
--    /   \     \
--   .     .     .  
--  / \   / \   / \ 
-- a   b c   d e   f

myITree :: H.InternalTree Int
myITree = H.IBranch 1 (H.IBranch 2 (H.IBranch 4 (H.ILeaf)
                                                (H.ILeaf))
                                   (H.IBranch 5 (H.ILeaf)
                                                (H.ILeaf)))
                      (H.IBranch 3 (H.ILeaf)
                                   (H.ILeaf) )


main :: IO ()
main = do putStrLn (show (H.rectangle 2 3))
          putStrLn (show (H.rtTriangle 2 3))
          putStrLn (show (H.sides (H.Ellipse 4 5)))
          putStrLn (show (H.sides (H.Polygon[])))
          putStrLn (show (H.sides (H.Polygon[(0, 1)])))
          putStrLn (show (H.sides (H.Polygon[(0, 1), (2, 3)])))
          putStrLn (show (H.sides (H.Polygon[(0, 1), (2, 3), (4, 5)])))
          putStrLn (show (H.sides (H.Polygon[(0, 1), (2, 3), (4, 5), (6, 7)])))
          putStrLn (show (H.sides (H.Polygon[(0, 1), (2, 3), (4, 5), (6, 7), (8, 9)])))
          putStrLn (show (H.bigger (H.Rectangle 2 3) 4))
          putStrLn (show (H.bigger (H.Ellipse 2 3) 5))
          putStrLn (show (H.bigger (H.RtTriangle 2 3) 9))
          putStrLn (show (H.bigger (H.Polygon[(0, 1)]) 2))
          putStrLn (show (H.bigger (H.Polygon[(0, 1), (2, 3)]) 4))
          putStrLn (show (H.bigger (H.Polygon[(0, 1), (2, 3), (4, 5)]) 4.5))
          putStrLn (show (H.bigger (H.Polygon[(0, 1), (2, 3), (4, 5), (6, 7)]) 1.21))
          putStrLn (show (H.bigger (H.Polygon[(0, 1), (2, 3), (4, 5), (6, 7), (8, 9)]) 9))
          H.hanoi 2 "a" "b" "c"
          H.hanoi 3 "x" "y" "z"
          putStrLn (show (H.lengthNonRecrusive ["a", "b", "c"]))
          putStrLn (show (H.doubleEach [1..10]))
          putStrLn (show (H.doubleEachNonRecursive [1..10]))
          putStrLn (show (H.pairAndOne [5..10]))
          putStrLn (show (H.pairAndOneNonRecursive [5..10]))
          putStrLn (show (H.addEachPair [(0, 1), (2, 3), (4, 5), (6, 7), (8, 9)]))
          putStrLn (show (H.addEachPairNonRecursive [(0, 1), (2, 3), (4, 5), (6, 7), (8, 9)]))
          putStrLn (show (H.minList [7]))
          putStrLn (show (H.minList [7, 5]))
          putStrLn (show (H.minList [19, 8, 5, 2, 1, -2, 7]))
          putStrLn (show (H.minListNonRecursive [7]))
          putStrLn (show (H.minListNonRecursive [7, 5]))
          putStrLn (show (H.minListNonRecursive [19, 8, 5, 2, 1, -2, 7]))
          putStrLn (show (H.maxList [7]))
          putStrLn (show (H.maxList [5, -5]))
          putStrLn (show (H.maxList [19, 8, 5, 2, 1, -2, 7]))
          putStrLn (show (H.maxListNonRecursive [7]))
          putStrLn (show (H.maxListNonRecursive [5, -5]))
          putStrLn (show (H.maxListNonRecursive [19, 8, 5, 2, 1, -2, 7]))
          putStrLn (show (H.fringe myTree))
          putStrLn (show (H.treeSize myTree))
          putStrLn (show (H.treeHeight myTree))
          putStrLn (show (H.takeTree 3 myITree))
          putStrLn (show (H.takeTree 2 myITree))
          putStrLn (show (H.takeTreeWhile (< 4) myITree))
          putStrLn (show (H.takeTreeWhile (< 5) myITree))
          putStrLn (show (H.myMap (+2) myList)) 
          putStrLn (show (H.myMap (*2) myList)) 
          H.sierpinskiCarpet
          H.myFractal
          H.mainXML
