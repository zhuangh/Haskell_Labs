module Main where

import Data.Map hiding (map, foldr)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Test.QuickCheck
import Control.Monad (forM, forM_)
import Data.List (transpose, intercalate)


import Final  


myBST :: BST Int Char
myBST = (Bind 10 'd' (Bind 5 'w'  Emp  Emp) (Bind 12 'a' Emp Emp) ) 

myBST1 = Bind 20 'f' (Bind 11 'x' (Bind 10 'd'  (Emp) (Bind 2 'a' Emp Emp) )  Emp ) Emp
myBST2 :: BST Int Char 
myBST2  = Bind 10 'a' (Bind 5 'b' Emp (Bind 7 'c' Emp Emp) ) 
	    (Bind 12 'z' (Bind 11 'b' (Bind 12 'q' Emp Emp) Emp) Emp )
myBST3 :: BST Int Char 
myBST3  = Bind 10 'a' (Bind 5 'b' Emp (Bind 7 'c' Emp Emp) ) 
	    (Bind 12 'z' (Bind 11 'b' Emp  Emp) Emp )

myBST4 = Bind 12 'a' (Bind 10 'b' (Bind 5 'c' Emp Emp) Emp) Emp
myBST5 = Bind 12 'a' (Bind 10 'b' Emp (Bind 11 'c' Emp Emp) ) Emp

r   = bstRotL (Bind 20 'a' (bstRotR (Bind 5 'b' Emp (Bind 15 'c' Emp Emp))) Emp)
rr  = bstInsert 14 'c' r
rrr = bstInsert 12 'x' rr
r4 = bstInsert 14 'x' rrr
r5 = bstInsert 3 'z' r4
r6 = bstInsert 19 'z' r5
r7 = bstInsert 18 'o' r6



main :: IO ()
main = do
     putStrLn $ show "Good Final" 
{--
     putStrLn $ show $ H.isBal (myBST1)
     putStrLn $ show $ H.toBinds (myBST)
     putStrLn $ show $ H.toBinds (myBST1)
     putStrLn $ show $ H.isBSO (myBST)
     putStrLn $ show $ H.isBSO (myBST1)
     putStrLn $ show $ H.bstDelete 10 (myBST)
     putStrLn $ show $ (myBST)
     putStrLn $ show $ H.bstInsert 11 'x' (myBST)
     quickCheck H.prop_insert_bso 
     quickCheck H.prop_insert_map
     quickCheck H.prop_delete_bso 
     quickCheck H.prop_delete_map 
--}
     putStrLn $ show $ height (myBST)
     putStrLn $ show $ isBal (myBST1)
     putStrLn $ show $ toBinds (myBST)
     putStrLn $ show $ toBinds (myBST1)
     putStrLn $ show $ isBSO (myBST)
     putStrLn $ show $ isBSO (myBST1)
     putStrLn $ show $ bstDelete 10 (myBST)
     putStrLn $ show $ (myBST)
     -- putStrLn $ show $ bstInsert 11 'x' (myBST)
     quickCheckN 1000 prop_insert_bso 
     quickCheckN 1000 prop_insert_map
     quickCheckN 1000 prop_delete_bso 
     quickCheckN 1000 prop_delete_map 


     putStrLn $ show $ bsthd myBST2

     {-- 
     putStrLn $ show $  ( bstInsert 6 'a' myBST3) 
     putStrLn $ show $ bstRotR r 
     putStrLn $ show $ isBal (bstInsert 6 'a' myBST3 )
     

     quickCheck prop_insert_bal
     quickCheck prop_delete_bal
     quickCheck prop_genBal
     putStrLn $ show $ r
     putStrLn $ show $ rr
     putStrLn $ show $ rrr
     putStrLn $ show $ r4
     putStrLn $ show $ r5
     putStrLn $ show $ r6
     putStrLn $ show $ r7
     putStrLn $ show $ bstDelete 15 r5 


     quickCheckN 1000 prop_insert_bal
     quickCheckN 1000 prop_delete_bal
     quickCheckN 1000 prop_genBal
     -- show testFiniteChannel 
     test7a 10 

     putStrLn $ show $ eLen
     putStrLn $ show $ eTl
     putStrLn $ show $ eInc     
     putStrLn $ show $ ePlus     
     putStrLn $ show $ eZero     
     putStrLn $ show $ eOne    
     putStrLn $ show $ eList    
     putStrLn $ show $ tHd
     putStrLn $ show $ eMap
     putStrLn $ show $ eSwap
     putStrLn $ show $ tSwap
     -- mapM_ test [e0, e1, e2, e3, e4, e5]

     --}
     quickCheckN 100 prop_insert_bal
     quickCheckN 100 prop_delete_bal
     quickCheckN 100 prop_genBal
     quickCheckN 100 prop_insert_bso 
     quickCheckN 100 prop_insert_map
     quickCheckN 100 prop_delete_bso 
     quickCheckN 100 prop_delete_map 

