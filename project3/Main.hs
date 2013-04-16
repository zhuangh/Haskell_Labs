module Main where

import Data.Map
import Control.Monad.State hiding (when)
import Text.Parsec hiding (State, between)
import Text.Parsec.Combinator hiding (between)
import Text.Parsec.Char
import Text.Parsec.String
import qualified Hw3 as H

-- ==> Do we need to use applicative functor to remove liftX in our program?

varA :: H.Variable
varB :: H.Variable
valA :: H.Value
valB :: H.Value
val0 :: H.Value
val5 :: H.Value
val7 :: H.Value

varA = "A"
varB = "B"
valA = H.IntVal 2
valB = H.IntVal 3
val0 = H.IntVal 0
val5 = H.IntVal 5
val7 = H.IntVal 7

memory :: H.Store
memory = fromList [(varA, valA), (varB, valB)] -- create a Map from a List

-- Parsec: http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf
run :: Show a => Parser a -> String -> IO ()
run p input
      = case (parse p "" input) of
          Left err -> do{ putStr "parse error at "
                        ; print err
                        }
          Right x -> print x

simple :: Parser Char
simple = letter

parens :: Parser ()
parens = do char '('
            parens
            char ')'
            parens
        <|> return ()

main :: IO ()
main = do
     H.playPong
--   putStrLn $ show varA
--   putStrLn $ show varB
--   putStrLn $ show valA
--   putStrLn $ show valB
--   putStrLn $ show memory
--   putStrLn $ ""
--   putStrLn $ show $ runState (H.evalE (H.Var "B")) memory
--   putStrLn $ show $ runState (H.evalE (H.Var "X")) memory
--   putStrLn $ show $ runState (H.evalE (H.Val valA)) memory
--   putStrLn $ show $ runState (H.evalE (H.Val (H.BoolVal True))) memory
--   putStrLn $ ""
--   putStrLn $ show $ runState (H.evalE (H.Op H.Plus (H.Var "B") (H.Var "A"))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Minus (H.Var "B") (H.Var "A"))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Times (H.Var "B") (H.Var "A"))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Divide (H.Var "B") (H.Var "A"))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Divide (H.Var "B") (H.Var "A"))) memory
---- putStrLn $ show $ runState (H.evalE (H.Op H.Divide (H.Var "B") (H.Val val0))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Gt (H.Var "B") (H.Val valB))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Ge (H.Var "B") (H.Val valB))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Lt (H.Var "A") (H.Val valA))) memory
--   putStrLn $ show $ runState (H.evalE (H.Op H.Le (H.Var "A") (H.Val valA))) memory
--   putStrLn $ ""
--   putStrLn $ show $ runState (H.evalS H.Skip) memory
--   putStrLn $ ""
--   putStrLn $ show $ runState (H.evalS $ H.Assign "A" (H.Val val5)) memory
--   putStrLn $ show $ runState (H.evalS $ H.Assign "C" (H.Val val7)) memory
--   putStrLn $ ""
--   H.run H.w_test
--   H.run H.w_fact
--   putStrLn $ ""
--   run simple "abcd"
--   run parens "()"
--   run parens "(()()())"
--   run parens "abcd"
-- --run parens "(())())"
-- --run parens "(()(())"
--   putStrLn $ ""
--   run H.intP "100"
--   run H.intP "010"
--   run H.intP "123; "
-- --run H.intP "= 123; "
--   run H.boolP "true"
--   run H.boolP "false"
--   run H.boolP "trueABC"
--   run H.boolP "true + 1;"
-- --run H.boolP "; + false"
--   putStrLn $ ""
--   run H.opP "+ 3"
--   run H.opP ">= 5"
   --run H.opP "& 5"
     run H.exprP "if X < 0 then X := 1; else X := 3; endif "
     run H.exprP "(X + 5) * Y"
     run H.exprP "X + (5 * Y)"
     run H.exprP "((1 + 2) - 3) + (1 + 3)"
     run H.exprP " Y + ( 3 + ( Z + U ))"
     run H.exprP " Y + ( 3 + ( Z + U) ) "
     run H.exprP "X"
     run H.exprP "(X)"
     run H.exprP "(5)"
     run H.exprP "(X + 5)"
     run H.exprP "(X + 5) * Y"
     run H.exprP "((X + 5) * Y)"
     run H.exprP "((X + 5) * Y) + (X / 5)"
     run H.exprP "(((X + 5) * Y) + (X / 5))"
     run H.exprP "(((X + 5) * Y) + ((X) / (5)))"
     putStrLn $ ""
     -- H.run H.w_test
     -- H.runFile "test.imp"
     putStrLn $ ""
     -- H.run H.w_fact
     -- H.runFile "fact.imp"
     putStrLn $ ""
     H.runFile "abs.imp"
     -- H.runFile "times.imp"
     putStrLn $ ""
     -- H.runFile "try.imp"
