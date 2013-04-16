module Main where

import Data.Map hiding (map, foldr)

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer

import Test.QuickCheck
import Control.Monad (forM, forM_)
import Data.List (transpose, intercalate)

{- import qualified Hw4 as H -}
import Hw4

varA :: Variable
varB :: Variable
valA :: Value
valB :: Value
val0 :: Value
val5 :: Value
val7 :: Value

varA = "A"
varB = "B"
valA = IntVal 2
valB = IntVal 3
val0 = IntVal 0
val5 = IntVal 5
val7 = IntVal 7

memory :: Store
memory = fromList [(varA, valA), (varB, valB)] -- create a Map from a List

mytest01 = mksequence [Assign "X" $ Val $ IntVal 0,
                       Assign "Y" $ Val $ IntVal 5]

mytest02 = mksequence [Assign "X" $ Val $ IntVal 0,
                       Assign "Y" $ Val $ IntVal 5,
                       Assign "Z" $ Op Plus (Var "X") (Var "Y")]

mytest03 = mksequence [Assign "X" $ Val $ IntVal 0,
                       Assign "Y" $ Val $ IntVal 5,
                       Assign "Z" $ Op Divide (Var "X") (Var "Y")]

mytest04 = mksequence [Assign "X" $ Val $ IntVal 3,
                       Assign "Y" $ Val $ IntVal 5,
                       Assign "Z" $ Op Divide (Var "Y") (Op Minus (Var "X") (Var "X"))]

mytest05 = mksequence [Assign "X" $ Val $ IntVal 0,
                       Assign "Y" $ Val $ IntVal 5,
                       Assign "W" $ Val $ BoolVal True,
                       Assign "Z" $ Op Divide (Var "Y") (Var "W")]

mytest06 = mksequence [Assign "X" $ Val $ IntVal 0,
                       Print "hello world: " $ Var "X",
                       Assign "Y" $ Val $ IntVal 5,
                       If (Op Minus (Var "X") (Var "Y")) (Throw (Op Plus (Var "X") (Var "Y"))) Skip,
                       Assign "Z" $ Val $ IntVal 3]

mytest07 = mksequence [Assign "X" $ Val $ IntVal 0,
                       Assign "Y" $ Val $ IntVal 5,
                       Assign "Z" $ Op Plus (Var "X") (Var "W")]


main :: IO ()
main = do
     putStrLn $ show valA
     putStrLn ""
     putStrLn $ show $ execute empty testprog1
     putStrLn $ show $ execute empty testprog2
     putStrLn ""
     putStrLn $ show $ execute empty mytest01
     putStrLn $ show $ execute empty mytest02
     putStrLn $ show $ execute empty mytest03
     putStrLn $ show $ execute empty mytest04
     putStrLn $ show $ execute empty mytest05
     putStrLn $ show $ execute empty mytest06
     putStrLn $ show $ execute empty mytest07
     putStrLn ""
     putStrLn "binary 101 -> Int 5:"
     putStrLn $ show $ binary [True, False, True]
     putStrLn ""
     putStrLn "sample1    101 -> True"
     putStrLn $ show $ sample1 (Sig [True, False, True])
     putStrLn ""
     putStrLn "sampleAt 1 101 -> False"
     putStrLn $ show $ sampleAt 0 (Sig [True, False, True])
     putStrLn ""
     putStrLn "test1a - fulladd:"
     test1a
     putStrLn ""
     putStrLn "test1 - bitAdder:"
     test1
     putStrLn ""
     putStrLn "quickCheck prop_bitAdder_Correct:"
     quickCheck prop_bitAdder_Correct
     putStrLn ""
     putStrLn "test2 - adder:"
     test2
     putStrLn ""
     putStrLn "quickCheck prop_Adder_Correct:"
     quickCheck prop_Adder_Correct
     putStrLn ""
     putStrLn "test3, test4, test5 - bitSubtractor:"
     test3
     test4
     test5
     putStrLn ""
     putStrLn "quickCheck prop_bitSubtractor_Correct:"
     quickCheck prop_bitSubtractor_Correct
     putStrLn ""
     putStrLn "test6, test7 - multiplier:"
     test6
     test7
     putStrLn ""
     putStrLn "quickCheck prop_Multiplier_Correct:"
     quickCheck prop_Multiplier_Correct
