---
title: Homework #3, Due Sunday, February 17th
---

Preliminaries
=============

Before starting this part of the assignment:

1. Install `parsec3` via the command `cabal install parsec3`
2. Learn to read the [documentation](http://hackage.haskell.org)
3. Download the test files
   [test.imp](/static/test.imp),
   [fact.imp](/static/fact.imp),
   [abs.imp](/static/abs.imp),
   [times.imp](/static/times.imp).


Submission Instructions
=======================

To complete this homework, download [this file as plain text](hw3.lhs) and
answer each question, filling in code where noted (where it says "TBD").
Your code must typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW3"; you
will receive a confirmation email after submitting.  Please note that
this address is unmonitored; if you have any questions about the
assignment, email Pat at `prondon@cs.ucsd.edu`.

> {-# LANGUAGE TypeSynonymInstances #-}
> module Hw3 where
> import Fal hiding (between, pball, walls, paddle)
> import Animation (picToGraphic)
> import qualified SOE as G
> import Picture
> import Data.Map
> import Control.Monad.State hiding (when)
> import Text.Parsec hiding (State, between)
> import Text.Parsec.Combinator hiding (between)
> import Text.Parsec.Char
> import Text.Parsec.String

Problem 1 : Pong
================

For the first problem, extend the paddleball game we saw in class to a two
player [Pong](http://en.wikipedia.org/wiki/Pong).

![Pong ScreenShot](/static/pong.png)

Both players start with `0` points, and whenever a player misses the ball,
the other player gets a point. When the game begins, and after each player
wins a point, the score is shown and the game must continue after a key is
pressed. The game must continue till a player reaches

> maxscore = 5

at which point she is declared the winner. The top level game is issued by
the function

> playPong = reactimate "pong" $ pong 0 0 2.0

which renders the behavior

> pong ::  Integer -> Integer -> Float -> Behavior G.Graphic
> pong p1score p2score vel =
>   if p1score == maxscore then
>     lift0 $ G.text (0, 0) "Player 1 wins!"
>   else if p2score == maxscore then
>     lift0 $ G.text (0, 0) "Player 2 wins!"
>   else
>     lift0 (G.text (0, 0) $ show p1score ++ " vs. " ++ show p2score)
>     `untilB` key ->> play p1score p2score vel

Your task is to fill in the implementation of following function

> play :: Integer -> Integer -> Float -> Behavior G.Graphic
> play p1score p2score vel = lift1 picToGraphic (walls `over` paddle1 `over` paddle2 `over` pongball)
>                            `switch` ( (p1miss ->> pong p1score (p2score + 1) vel) .|. (p2miss ->> pong (p1score + 1) p2score vel) )
>   where pongball = (paint yellow $ translate (xpos, ypos) ball)
>         ball   = ell 0.2 0.2
>         xpos   = integral xvel
>         ypos   = integral yvel
>         xvel   = vel `stepAccum` xhit ->> negate
>         yvel   = vel `stepAccum` yhit ->> negate
>         xhit   = when (xpos >*  2 ||* xpos <* -2)
>         yhit   = p1hit .|. p2hit
>         p1hit  = when (ypos        `between` (1.5,   1.7) &&*
>                        fst mouse   `between` (xpos - 0.45, xpos + 0.45))
>         p2hit  = when (ypos        `between` (-1.7, -1.5) &&*
>                        keyboardPos `between` (xpos - 0.45, xpos + 0.45))
>         ymiss  = p1miss .|. p2miss
>         p1miss = when (ypos <* -2.5)
>         p2miss = when (ypos >* 2.5)
>         x `between` (a, b) = x >* a &&* x <* b


Use the same conditions as in `paddleball` to determine when the ball has
hit the paddle. The ball can be said to have missed player 1's paddle (ie
player 2 scores a point) when the ball's y-coordinate

~~~~~~{.haskell}
ypos <* -2.5
~~~~~~

dually, player 1 scores a point when

~~~~~~{.haskell}
ypos >* 2.5
~~~~~~

You may use the following behaviors to render the wall,

> walls = left `over` right
>   where left  = paint blue $ translate (-2.2,0) (rec 0.05 3.4)
>         right = paint blue $ translate ( 2.2,0) (rec 0.05 3.4)

the paddles for each player,

> paddle1 ::  Behavior Picture
> paddle1 = paddle (-1.7) red p1input
>
> paddle2 ::  Behavior Picture
> paddle2 = paddle 1.7 green p2input
>
> paddle :: Behavior Float-> Behavior Color-> Behavior Float-> Behavior Picture
> paddle y color pos = paint color $ translate (pos, y) (rec 0.5 0.05)

The positions of the paddles of each player are given by the following
behaviors

> p1input ::  Behavior Float
> p1input = keyboardPos
> p2input ::  Behavior Float
> p2input = fst mouse

which are generated thus (you can ignore this if you are not curious...)

> keyUpE k = Event (\(uas,_) -> Prelude.map getkey uas)
>   where getkey (Just (G.Key k' False)) | k' == k = Just ()
>         getkey _                               = Nothing
>
> kbSpeed = 2.5
> keyboardVel = lift0 0 `switch` key =>> \k ->
>   case k of
>     'a' -> lift0 (-kbSpeed) `untilB` (keyUpE 'a') ->> lift0 0
>     'd' -> lift0 kbSpeed `untilB` (keyUpE 'd') ->> lift0 0
>     _   -> lift0 0
>
> keyboardPos = integral keyboardVel


Problem 2: An Interpreter for WHILE
===================================

Next, you will use monads to build an evaluator for
a simple *WHILE* language. In this language, we will
represent different program variables as

> type Variable = String

Programs in the language are simply values of the type

> data Statement =
>     Assign Variable Expression          -- x = e
>   | If Expression Statement Statement   -- if (e) {s1} else {s2}
>   | While Expression Statement          -- while (e) {s}
>   | Sequence Statement Statement        -- s1; s2
>   | Skip                                -- no-op
>   deriving (Show)

where expressions are variables, constants or
binary operators applied to sub-expressions

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v
>   | Op  Bop Expression Expression
>   deriving (Show)

and binary operators are simply two-ary functions

> data Bop =
>     Plus     -- +  :: Int  -> Int  -> Int
>   | Minus    -- -  :: Int  -> Int  -> Int
>   | Times    -- *  :: Int  -> Int  -> Int
>   | Divide   -- /  :: Int  -> Int  -> Int
>   | Gt       -- >  :: Int -> Int -> Bool
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Show)

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Show)

We will represent the *store* i.e. the machine's memory, as an associative
map from `Variable` to `Value`

> type Store = Map Variable Value

**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return
the value `0`. In future assignments, we will add this as a
case where exceptions are thrown (the other case being type errors.)

We will use the standard library's `State`
[monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer
`s -> (a, s)`. See the above documentation for more details.
You can ignore the bits about `StateT` for now.

Expression Evaluator
--------------------

First, write a function

> evalE :: Expression -> State Store Value

that takes as input an expression and returns a world-transformer that
returns a value. Yes, right now, the transformer doesnt really transform
the world, but we will use the monad nevertheless as later, the world may
change, when we add exceptions and such.

**Hint:** The value `get` is of type `State Store Store`. Thus, to extract
the value of the "current store" in a variable `s` use `s <- get`.

> evalE (Var x)      = do
>                    memory <- get
>                    return (findWithDefault (IntVal 0) x memory)
> evalE (Val v)      = do
>                    return v
> evalE (Op o e1 e2) = do
>                    val1 <- evalE e1
>                    val2 <- evalE e2
>                    case o of
>                      Plus -> return $ val1 `plus` val2
>                      Minus -> return $ val1 `minus` val2
>                      Times -> return $ val1 `times` val2
>                      Divide -> return $ val1 `divide` val2
>                      Gt -> return $ val1 `gt` val2
>                      Ge -> return $ val1 `ge` val2
>                      Lt -> return $ val1 `lt` val2
>                      Le -> return $ val1 `le` val2
>     where plus   = (\(IntVal v1) (IntVal v2) -> IntVal (v1 + v2))
>           minus  = (\(IntVal v1) (IntVal v2) -> IntVal (v1 - v2))
>           times  = (\(IntVal v1) (IntVal v2) -> IntVal (v1 * v2))
>           divide = (\(IntVal v1) (IntVal v2) -> IntVal (v1 `safediv` v2))
>           gt     = (\(IntVal v1) (IntVal v2) -> BoolVal (v1 > v2))
>           ge     = (\(IntVal v1) (IntVal v2) -> BoolVal (v1 >= v2))
>           lt     = (\(IntVal v1) (IntVal v2) -> BoolVal (v1 < v2))
>           le     = (\(IntVal v1) (IntVal v2) -> BoolVal (v1 <= v2))
>           safediv n m = if m == 0 then 0 else n `div` m



Statement Evaluator
-------------------

Next, write a function

> evalS :: Statement -> State Store ()

that takes as input a statement and returns a world-transformer that
returns a unit. Here, the world-transformer should in fact update the input
store appropriately with the assignments executed in the course of
evaluating the `Statement`.

**Hint:** The value `put` is of type `Store -> State Store ()`.
Thus, to "update" the value of the store with the new store `s'`
do `put s`.

> evalS w@(While e s)    = do
>                        cond <- evalE e
>                        case cond of
>                          BoolVal True -> evalS (Sequence s w)
>                          otherwise -> evalS Skip
> evalS Skip             = do
>                        memory <- get
>                        put memory
> evalS (Sequence s1 s2) = do
>                        evalS s1
>                        evalS s2
> evalS (Assign x e )    = do
>                        memory <- get
>                        val <- evalE e
>                        put (insert x val memory)
> evalS (If e s1 s2)     = do
>                        cond <- evalE e
>                        case cond of
>                          BoolVal True -> evalS s1
>                          BoolVal False -> evalS s2
>                          otherwise -> evalS Skip

In the `If` case, if `e` evaluates to a non-boolean value, just skip both
the branches. (We will convert it into a type error in the next homework.)
Finally, write a function

> execS :: Statement -> Store -> Store
> execS stmt store = execState (evalS stmt) store

such that `execS stmt store` returns the new `Store` that results
from evaluating the command `stmt` from the world `store`.
**Hint:** You may want to use the library function

~~~~~{.haskell}
execState :: State s a -> s -> s
~~~~~

When you are done with the above, the following function will
"run" a statement starting with the `empty` store (where no
variable is initialized). Running the program should print
the value of all variables at the end of execution.

> run :: Statement -> IO ()
> run stmt = do putStrLn "Output Store:"
>               putStrLn $ show $ execS stmt empty

Here are a few "tests" that you can use to check your implementation.

> w_test = (Sequence (Assign "X" (Op Plus (Op Minus (Op Plus (Val (IntVal 1)) (Val (IntVal 2))) (Val (IntVal 3))) (Op Plus (Val (IntVal 1)) (Val (IntVal 3))))) (Sequence (Assign "Y" (Val (IntVal 0))) (While (Op Gt (Var "X") (Val (IntVal 0))) (Sequence (Assign "Y" (Op Plus (Var "Y") (Var "X"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1))))))))

> w_fact = (Sequence (Assign "N" (Val (IntVal 2))) (Sequence (Assign "F" (Val (IntVal 1))) (While (Op Gt (Var "N") (Val (IntVal 0))) (Sequence (Assign "X" (Var "N")) (Sequence (Assign "Z" (Var "F")) (Sequence (While (Op Gt (Var "X") (Val (IntVal 1))) (Sequence (Assign "F" (Op Plus (Var "Z") (Var "F"))) (Assign "X" (Op Minus (Var "X") (Val (IntVal 1)))))) (Assign "N" (Op Minus (Var "N") (Val (IntVal 1))))))))))

As you can see, it is rather tedious to write the above tests! They
correspond to the code in the files `test.imp` and `fact.imp`. When you are
done, you should get

~~~~~{.haskell}
ghci> run w_test
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> run w_fact
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

Problem 3: A Parser for WHILE
=============================

It is rather tedious to have to specify individual programs as Haskell
values. For this problem, you will use parser combinators to build a parser
for the WHILE language from the previous problem.

Parsing Constants
-----------------

First, we will write parsers for the `Value` type

> valueP :: Parser Value
> valueP = intP <|> boolP

To do so, fill in the implementations of

> intP :: Parser Value
> intP = do
>        try (char '-')
>        skipMany space
>        str <- many1 digit
>        return $ IntVal $ 0 - (read str)
>        <|> do
>            str <- many1 digit
>            return $ IntVal (read str)
>  <?> "a decimal digit 0..9 or a single '-' before all digits"


Next, define a parser that will accept a
particular string `s` as a given value `x`

> constP :: String -> a -> Parser a
> constP s x = do
>            string s
>            return x

and use the above to define a parser for boolean values
where `"true"` and `"false"` should be parsed appropriately.

> boolP :: Parser Value
> boolP = do
>       constP "true"  (BoolVal True)
>   <|> constP "false" (BoolVal False)
>   <?> "true or false"

Continue to use the above to parse the binary operators

> opP :: Parser Bop
> opP = do
>       try $ constP "<=" Le
>       <|> do
>           try $ constP ">=" Ge
>           <|> constP "+" Plus
>           <|> constP "-" Minus
>           <|> constP "*" Times
>           <|> constP "/" Divide
>           <|> constP ">" Gt
>           <|> constP "<" Lt
>   <?> "+ or - or * or / or > or >= or < or <="


Parsing Expressions
-------------------

Next, the following is a parser for variables, where each
variable is one-or-more uppercase letters.

> varP :: Parser Variable
> varP = many1 upper

Use the above to write a parser for `Expression` values

> exprVar :: Parser Expression
> exprVar = do
>         var <- varP
>         return (Var var)
>     <?> "Variable"

> exprVal :: Parser Expression
> exprVal = do
>         val <- valueP
>         return (Val val)
>     <?> "Value"

> exprInParens :: Parser Expression
> exprInParens = do
>              char '('
>              skipMany space
>              expr <- exprP
>              skipMany space
>              char ')'
>              return expr
>          <?> "an expression in parenthesis"

> exprBop :: Parser Expression
> exprBop = do
>         skipMany space
>         lhs <- exprVar <|> exprVal <|> exprInParens
>         skipMany space
>         op <- opP
>         skipMany space
>         rhs <- exprVar <|> exprVal <|> exprInParens
>         skipMany space
>         return (Op op lhs rhs)
>     <?> "a Bop Expression Expression"

> exprP :: Parser Expression
> exprP = do
>         try $ exprBop
>         <|> do
>             try $ exprInParens
>             <|> exprVar
>             <|> exprVal
>     <?> "a valid expression"


Parsing Statements
------------------

Next, use the expression parsers to build a statement parser


> assignP :: Parser Statement
> assignP = do
>           skipMany space
>           var <- varP
>           skipMany space
>           string ":="
>           skipMany space
>           expr <- exprP
>           return $ Assign var expr

> ifP :: Parser Statement
> ifP = do
>       skipMany space
>       string "if"
>       skipMany space
>       cond <- exprP
>       skipMany space
>       string "then"
>       skipMany space
>       s1 <- statementP
>       skipMany space
>       string "else"
>       skipMany space
>       s2 <- statementP
>       skipMany space
>       string "endif"
>       return $ If cond s1 s2

> whileP :: Parser Statement
> whileP = do
>        skipMany space
>        string "while"
>        skipMany space
>        cond <- exprP
>        skipMany space
>        string "do"
>        skipMany space
>        stmt <- statementP
>        skipMany space
>        string "endwhile"
>        return $ While cond stmt

> seqP :: Parser Statement
> seqP = do
>      skipMany space
>      s1 <- assignP <|> ifP <|> whileP <|> skipP
>      skipMany space
>      char ';'
>      skipMany space
>      s2 <- statementP
>      return $ Sequence s1 s2

> skipP :: Parser Statement
> skipP = do
>       string "skip"
>       return $ Skip

> statementP :: Parser Statement
> statementP = do
>              try seqP
>              <|> assignP
>              <|> ifP
>              <|> whileP
>              <|> skipP

When you are done, we can put the parser and evaluator together
in the end-to-end interpreter function

> runFile s = do p <- parseFromFile statementP s
>                case p of
>                  Left err   -> print err
>                  Right stmt -> run stmt

When you are done you should see the following at the ghci prompt

~~~~~{.haskell}
ghci> runFile "test.imp"
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> runFile "fact.imp"
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

