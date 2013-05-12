!!! For your own good, do not read my crappy code when you are taking this courses. 


---
title: Final, Due Sun 3/24
---

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, OverlappingInstances, FlexibleInstances #-}

> module Final where
> import qualified Data.Map as Map 
> import Control.Monad.State
> import Control.Monad.Error
> import Control.Monad.Writer
> import Test.QuickCheck 
> import Control.Monad (forM, forM_)
> import Data.List (transpose, intercalate)
>
> import qualified Data.Set as Set
> import Control.Applicative ((<$>))
> import qualified Text.PrettyPrint as PP
>
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import System.Exit
> import System.IO.Unsafe
> import Data.IORef
>
> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n}




Submission Instructions
=======================

To complete this homework, download [this file as plain text](final.lhs) and
answer each question, filling in code where
noted (some questions ask for explanations in addition to or instead
of code).

**Your code must typecheck against the given type signatures.**

Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "Final"; you
will receive a confirmation email after submitting.  Please note that
this address is unmonitored; if you have any questions about the
assignment, email Pat at `prondon+230@cs.ucsd.edu`.

**Note** You may have to `cabal install stm` and `cabal install quickcheck`.

Problem 1: Binary Search Trees Revisited
========================================

Recall the old type of binary search trees from
[HW2](/homeworks/hw2.html).

> data BST k v = Emp 
>              | Bind k v (BST k v) (BST k v) 
>              deriving (Show)
> 
> toBinds ::  BST t t1 -> [(t, t1)]
> toBinds Emp            = []
> toBinds (Bind k v l r) = toBinds l ++ [(k,v)] ++ toBinds r

The following function tests whether a tree satisfies the 
binary-search-order invariant.

> isBSO ::  Ord a => BST a b -> Bool
> isBSO Emp            = True
> isBSO (Bind k v l r) = all (< k) lks && all (k <) rks && isBSO l && isBSO r
>   where lks = map fst $ toBinds l
>         rks = map fst $ toBinds r

Finally, to test your implementation, we will define a 
type of operations over trees

> data BSTop k v = BSTadd k v | BSTdel k 
>                  deriving (Eq, Show)

and a function that constructs a tree from a sequence of operations

> ofBSTops ::  Ord k => [BSTop k v] -> BST k v
> ofBSTops    = foldr doOp Emp
>   where doOp (BSTadd k v) = bstInsert k v 
>         doOp (BSTdel k)   = bstDelete k 

and that constructs a reference `Map` from a sequence of operations

> mapOfBSTops ::  Ord k => [BSTop k a] -> Map.Map k a
> mapOfBSTops = foldr doOp Map.empty 
>   where doOp (BSTadd k v) = Map.insert k v
>         doOp (BSTdel k)   = Map.delete k

and functions that generate an arbitrary BST operations

> keys :: [Int] 
> -- keys = [0..10]
> keys = [0..10]
>
> genBSTadd, genBSTdel, genBSTop ::  Gen (BSTop Int Char)
> genBSTadd = liftM2 BSTadd (elements keys) (elements ['a'..'z'])
> genBSTdel = liftM BSTdel (elements keys)
> genBSTop  = frequency [(5, genBSTadd), (1, genBSTdel)] 

(a) Insertion
-------------

The data structure of BST, I use AVL tree here. 

The right child rotation 

> bstRotR :: BST k v -> BST k v
> bstRotR Emp				    = Emp -- error "cannot rotate left children"
> bstRotR (Bind k v l  Emp)		    = error "cannot rotate right children as father"
> bstRotR (Bind k v l (Bind rk rv rl rr) )  = Bind rk rv (Bind k v l rl) rr 




The left child's rotation 

> bstRotL :: BST k v -> BST k v 
> bstRotL Emp				    = Emp -- error "cannot rotate right"
> bstRotL (Bind k v Emp r)		    = error "cannot rotate left children as father"
> bstRotL (Bind k v  (Bind lk lv ll lr) r ) = Bind  lk lv ll (Bind k v lr r)  

calculate height & diff value between left and right children 

> bsthd :: BST k v -> (Int, Int)
> bsthd (Bind k v l r) = (  1 + ( max h_l h_r), h_l - h_r  )
>			where h_l = height l
>			      h_r = height r
> bsthd Emp = (0, 0)

For the Balance of AVL tree via rotation to deal with 
The situation: 
The rotation from bottom (depth 2 for father) up
(1) LL : rotL
(2) LR : rotL and rotR
(3) RR : rotR
(4) RL : rotR and rotL

> bstRotBal ::  Int ->  Int -> Int -> Int -> Int -> BST k v -> Either ( BST k v) (Int, Int, BST k v) 
> -- BSTRot (Bind k v l r) diff_fa  diff_l height_l diff_r height_r  
> bstRotBal diff_fa  height_l diff_l   height_r diff_r (Bind k v l r)   
>		    | diff_fa > 1  &&  diff_l >= 0 =  Left $ bstRotL $ ( Bind k v l r)  
>		    | diff_fa > 1  &&  diff_l < 0 =  Left $ bstRotL $ ( Bind k v  (bstRotR l) r)
>		    | diff_fa < -1 &&  diff_r <= 0 =  Left $ bstRotR $ (Bind k v  l r )
>		    | diff_fa < -1 &&  diff_r > 0 =  Left $ bstRotR  $(Bind k v l (bstRotL r)  ) 
>		    | otherwise = Right ( hei , diff_fa  , (Bind k v  l r)  )
>		    where hei = 1 + max height_l height_r
>

Real Insertion via height and delta height parameters

> balInsert :: (Ord k) => k -> v -> BST k v -> 
>  	Either (BST k v ) (Int, Int, BST k v)
> balInsert k' v' Emp = Right (1, 0 ,( Bind k' v' Emp Emp))
> balInsert k' v' (Bind k v l r) 
>	| k' == k   = Left  (Bind k' v' l r)
>	| k' < k    = case balInsert k' v' l of
>			Right (height_l, diff_l , l' ) 
>			    -> bstRotBal (height_l- height_r) height_l diff_l height_r diff_r (Bind k v l' r)
>				where (height_r, diff_r) = bsthd r 
>			Left l'   
>			    -> Left $ (Bind k v l' r) 
>       | otherwise = case balInsert k' v' r of 
>			Right (height_r, diff_r , r' ) 
>			    -> bstRotBal (height_l- height_r) height_l diff_l height_r diff_r (Bind k v l r')
>				where (height_l, diff_l) = bsthd l 
>			Left r'   
>			    -> Left $ (Bind k v l r') 
> {--
> balInsert k' v' (Bind k v l r) 
>   |  k' == k  = Left $ (Bind k' v' l r)
>   |  k' <  k  = case balInsert k' v' l of 
>		    Right (height_l, diff_l, l') -> let ( height_r, diff_r) = bsthd r in
>							bstRot ( (height_l) - (height_r)) l' height_l diff_l r  height_r diff_r ( Bind k v ) 
>		    Left  l'                   -> Left $ Bind k v l' r
>   | otherwise = case balInsert k' v' r of 
>		    Right (height_r, diff_r, r') -> let ( height_l, diff_l) = bsthd l in
>							bstRot ( (height_l) - (height_r)) l height_l diff_l r'  height_r diff_r ( Bind k v ) 
>		    Left  r'                   -> Left $ Bind k v l  r'
>	where  bstRot diff_fa l height_l diff_l  r height_r diff_r avlbind   
>		    | diff_fa > 1  &&  diff_l >= 0 =  Left $ bstRotR  ( avlbind l r)  
>		    | diff_fa > 1  &&  diff_l < 0 =  Left $ bstRotR  ( avlbind  (bstRotL l) r )
>		    | diff_fa < -1 &&  diff_l <= 0 =  Left $ bstRotL  (avlbind l r )
>		    | diff_fa < -1 &&  diff_l > 0 =  Left $ bstRotL  (avlbind (bstRotR l) r) 
>	--	    | otherwise = Right (1 + max ((height_l), (height_r)) , diff_fa  , (avlbind  l r)  )
>		    | otherwise = Right ( hei , diff_fa  , (avlbind  l r)  )
>		    where hei = 1 + max height_l height_r
>    --}
>






Write an insertion function 

Use above real balInsert (AVL)

> bstInsert :: (Ord k) => k -> v -> BST k v -> BST k v
> bstInsert k v = either id ( \(k', v', bstI)-> bstI ) . balInsert k v 
> --  bstInsert = error "TBD"
> {--
> bstInsert :: (Ord k) => k -> v -> BST k v -> BST k v
> bstInsert kk vv Emp = Bind kk vv Emp Emp
> bstInsert kk vv (Bind k v l r) 
>     | kk > k = Bind k v l (bstInsert kk vv r)
>     | kk < k = Bind k v (bstInsert kk vv l) r
>     | kk == k = Bind kk vv l r  
> --}

such that `bstInsert k v t` inserts a key `k` with value 
`v` into the tree `t`. If `k` already exists in the input
tree, then its value should be *replaced* with `v`. When you 
are done, your code should satisfy the following QC properties.

> prop_insert_bso :: Property
> prop_insert_bso = forAll (listOf genBSTadd) $ \ops -> 
>                     isBSO (ofBSTops ops)
>
> prop_insert_map = forAll (listOf genBSTadd) $ \ops -> 
>                     toBinds (ofBSTops ops) == Map.toAscList (mapOfBSTops ops)

(b) Deletion
------------

Write a deletion function for BSTs of this type:




> maxNode :: BST k v -> (k , v)
> maxNode Emp               = error "Wrong input for maxNode"
> maxNode (Bind k v _ Emp ) = ( k, v )
> maxNode (Bind _ _ _ r   ) = maxNode r

> balbst :: BST k v -> BST k v
> balbst (Bind k v l r)  
>     | dlr (Bind k v l r) > 1 && dlr l >= 0 = bstRotL $ (Bind k v l r) 
>     | dlr (Bind k v l r) > 1 && dlr l < 0 = bstRotL $ (Bind k v (bstRotR l) r) 
>     | dlr (Bind k v l r) < -1 && dlr r <= 0 = bstRotR $ (Bind k v l r) 
>     | dlr (Bind k v l r) < -1 && dlr r > 0 = bstRotR $ (Bind k v l (bstRotL r)) 
>     | otherwise  = Bind k v l r
>         where dlr Emp = 0 
>		dlr (Bind k v l r) = height l - height r
>		

> bstDelete :: (Ord k) => k -> BST k v -> BST k v
> -- bstDelete k t = error "TBD"
> bstDelete _ Emp = Emp
> bstDelete k (Bind kk v l r) 
>    | k > kk = balbst $ (Bind kk v l (bstDelete k r))
>    | k < kk = balbst $ (Bind kk v (bstDelete k l) r)
>    | otherwise = case ( l, r) of 
>				(_  , Emp) -> l
>				(Emp, _  ) -> r
>				(_  , _  ) -> let ( kt , vt) = maxNode l
>					      in balbst $ (Bind kt vt (bstDelete kt l ) r)
> 


such that `bstDelete k t` removes the key `k` from the tree `t`. 
If `k` is absent from the input tree, then the tree is returned 
unchanged as the output. When you are done, your code should 
satisfy the following QC properties.

> prop_delete_bso :: Property
> prop_delete_bso = forAll (listOf genBSTop) $ \ops -> 
>                     isBSO (ofBSTops ops)
>
> prop_delete_map = forAll (listOf genBSTop) $ \ops -> 
>                     toBinds (ofBSTops ops) == Map.toAscList (mapOfBSTops ops)


(c) Balanced Trees  
------------------

The following function determines the `height` of a BST 

> height :: BST k v -> Int  -- add by examinee
> height (Bind _ _ l r) = 1 + max (height l) (height r)
> height Emp            = 0

We say that a tree is *balanced* if 

> isBal (Bind _ _ l r) = isBal l && isBal r && abs (height l - height r) <= 2
> isBal Emp            = True

Write a balanced tree generator 

> genBal :: Gen (BST Int Char)
> -- genBal = error "TBD"
> genBal = liftM ofBSTops . listOf $  genBSTadd 

such that

> prop_genBal = forAll genBal isBal

(d) Height Balancing (** Hard **) 
---------------------------------

Rig it so that your insert and delete functions *also*
create balanced trees. That is, they satisfy the properties

> prop_insert_bal ::  Property
> prop_insert_bal = forAll (listOf genBSTadd) $ isBal . ofBSTops 
>
> prop_delete_bal ::  Property
> prop_delete_bal = forAll (listOf genBSTop) $ isBal . ofBSTops


Problem 2: Concurrent FIFO Queue
================================

In this question, we'll develop a concurrently-accessible first-in,
first-out (FIFO) queue with a fixed capacity, called a finite channel.[^channelnote]
Your finite channel will behave as follows: If a read occurs when the
queue is empty, the reader should block until an item becomes
available. Similarly, if a write occurs when the queue is full (i.e.,
the number of items in the queue is the capacity specified when the
queue was created), the writer should block until an item is removed
from the queue.

[^channelnote]: This question is based on homeworks 10 and 11 from
Benjamin Pierce's 
[Advanced Programming](http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html)
class at UPenn.

Before defining any operations on your finite channel, you need to
change the representation of finite channels from the following
obviously incorrect one:

> -- data FiniteChan a = Chan ()
> data FiniteChan a = Chan Int ( TVar (  Int, [a])) 

Next, define an operation for creating a finite channel of a
particular capacity:

> newFiniteChan :: Int -> IO (FiniteChan a)
> -- newFiniteChan capacity = error "TODO"
> newFiniteChan capacity = do t <- atomically $ newTVar (0, [])
>			      return $ Chan capacity t



Next, define the operation for reading from the queue:

> readFiniteChan :: FiniteChan a -> IO a
> -- readFiniteChan t = error "TODO"
> readFiniteChan (Chan size t) = atomically $ do
> 	    ( pos, q  ) <- readTVar t
>	    if pos == 0
>		then retry 
> 	        else do writeTVar t ( pos-1, tail q)  
>		        return (head q)

Remember that reads should block in the case where the channel is
empty.

Finally, define the operation for writing to the queue:

> writeFiniteChan :: FiniteChan a -> a -> IO ()
> -- writeFiniteChan t x = error "TODO"
> writeFiniteChan (Chan size t) x = atomically $ do 
> 	(  pos, q ) <- readTVar t 
>	if pos == size 
>	   then retry
> 	   else writeTVar t ( pos+1, q ++ [x] )
>	

Remember that writes should block in the case where the channel is at
capacity.

Below are some tests that exercise your channel abstraction. You should
run the `testFiniteChannel' function at the very end to ensure that all
of the tests pass.

First, we define a debugging output function for tracing the execution
of the tests. Uncomment out the second version if you need more information
to diagnose errors.

> dbg s = do return ()
> -- dbg s = do id <- myThreadId;  putStrLn $ "[" ++ (show id) ++ "] " ++ s

Various test parameters and utilities follow.

> rounds = 1000

> delayUnit = 1000*200 -- 0.2 seconds

> assert b s = 
>   if b then return () else do
>   putStrLn $ "assertion failed: " ++ s
>   exitWith (ExitFailure 1)

> beginTest s = putStrLn $ "Starting test procedure: " ++ s

> endTest = do
>   putStrLn "Test passed"
>   putStrLn ""

> newFC x = do
>   dbg $ "newFiniteChan " ++ (show x) ++ " called"
>   c <- newFiniteChan x
>   dbg $ "newFiniteChan " ++ (show x) ++ " returned"
>   return c

> readFC c = do
>   dbg $ "readFiniteChan called"
>   x <- readFiniteChan c
>   dbg $ "readFiniteChan returned, result=" ++ (show x)
>   return x

> writeFC c x = do
>   dbg $ "writeFiniteChan " ++ (show x) ++ " called"
>   writeFiniteChan c x
>   dbg $ "writeFiniteChan " ++ (show x) ++ " returned"

The first test fills and empties the queue twice, checking that FIFO
order is respected.

> test1 = forM [1..5] $ \i -> test1a i

> test1a x = do
>   beginTest $ "test1:"++(show x)
>   fc <- newFC x
>   forM [1..x] $ \i-> writeFC fc i
>   forM [1..x] $ \i-> do
>     j <- readFC fc
>     assert (i==j) "FIFO order not respected"
> 
>   forM [x+1..2*x] $ \i-> writeFC fc i
>   forM [x+1..2*x] $ \i-> do
>     j <- readFC fc
>     assert (i==j) "FIFO order not respected"
> 
>   endTest

The second test is a simple two-thread producer/consumer setup, again
testing for FIFO semantics.

> test2 = forM [1..5] $ \i -> test2a i

> test2a size = do
>   beginTest $ "test2:"++(show size)
>   fc <- newFC size
>   forkIO (producer fc)
>   consumer fc
>   forkIO (consumer fc)
>   producer fc
>   endTest
>  where
>   producer fc = do
>     forM [1..rounds] $ \i-> do
>       writeFC fc i
>     return ()
>   consumer fc = do
>     forM [1..rounds] $ \i-> do
>       j <- readFC fc
>       assert(i==j) "FIFO order not respected"
>     return ()

The third test checks that, if the consumer is slow, the queue will
always be full when it's read, and also that the producer is not
allowed to insert more items into the queue than its capacity should
allow.

> test3 = forM [1..5] $ \i -> test3a i

> test3a size = do
>   beginTest $ "test3:"++(show size)
>   fc <- newFC size
>   forkIO (producer fc)
>   consumer fc
>   endTest
>  where
>   counter = unsafePerformIO (newMVar 0)
>   producer fc = do
>     forM [1..rounds] $ \i-> do
>       writeFC fc i
>       modifyMVar_ counter $ \c-> do
>         -- putStrLn $ (show c) ++ "<" ++ (show size)
>         assert (c<size) "Queue size not within limit"
>         return (c+1)
>     return ()
>   consumer fc = do
>     forM [1..10] $ \i-> do
>       threadDelay delayUnit
>       modifyMVar_ counter $ \c-> do
>          -- putStrLn $ (show c) ++ "<=" ++ (show size)
>          assert (c==size) "Queue should always be full with a slow reader"
>          return (c-1)
>       j <- readFC fc
>       assert(i==j) ""
>     return ()

The fourth test is like the third, except its checks that, with a slow
producer, the queue is always empty when it's written to.

> test4 = forM [1..5] $ \i -> test4a i

> test4a size = do
>   beginTest $ "test4:"++(show size)
>   fc <- newFC size
>   forkIO (consumer fc)
>   producer fc
>   endTest
>  where
>   counter = unsafePerformIO (newMVar 0)
>   producer fc = do
>     forM [1..5] $ \i-> do
>       threadDelay delayUnit
>       modifyMVar_ counter $ \c-> do
>         -- putStrLn $ (show c) ++ "<" ++ (show size)
>         assert (c==0) "Queue should always be empty with a slow writer"
>         return (c+1)
>       writeFC fc i
>     return ()
>   consumer fc = do
>     forM [1..rounds] $ \i-> do
>       j <- readFC fc
>       assert(i==j) ""
>       modifyMVar_ counter $ \c-> do
>          -- putStrLn $ (show c) ++ "<=" ++ (show size)
>          assert (c<=size) "Queue size not within limit"
>          return (c-1)
>     return ()

The fifth test checks the behavior of multiple producers and
consumers.

> test5 = forM [1..5] $ \i -> test5a i

> test5a size = do
>   beginTest $ "test5:"++(show size)
>   fc1 <- newFC size
>   fc2 <- newFC 1
>   forM [1..nums] $ \_ -> forkIO (producer fc1)
>   forM [1..nums] $ \_ -> forkIO (consumer fc1 fc2)
>   s <- newIORef 0
>   forM [1..(nums*rounds)] $ \_ -> do
>     i <- readFC fc2
>     modifyIORef s (+i)
>   result <- readIORef s
>   assert (result== nums * (sum [1..rounds])) "total sent <> total received"
>   endTest
>  where
>   nums = 10 
>   producer fc = do
>     forM [1..rounds] $ \i -> writeFC fc i
>     return ()
>   consumer fc1 fc2 = do
>     forM [1..rounds] $ \_ -> do
>       i <- readFC fc1
>       writeFC fc2 i
>     return ()

The sixth test is like the third, this time with multiple producer
threads.

> test6 = forM [1..5] $ \i -> test6a i

> test6a size = do
>   beginTest $ "test6:"++(show size)
>   fc <- newFC size
>   forM [1..5] $ \_ -> forkIO (producer fc)
>   consumer fc
>   endTest
>  where
>   counter = unsafePerformIO (newMVar 0)
>   producer fc = do
>     forM [1..rounds] $ \i-> do
>       writeFC fc i
>       modifyMVar_ counter $ \c-> do
>         -- putStrLn $ (show c) ++ "<" ++ (show size)
>         assert (c<size) "queue size not within limit"
>         return (c+1)
>     return ()
>   consumer fc = do
>     forM [1..10] $ \i-> do
>       threadDelay delayUnit
>       modifyMVar_ counter $ \c-> do
>          -- putStrLn $ (show c) ++ "<=" ++ (show size)
>          assert (c==size) "queue should always be full with slow reader"
>          return (c-1)
>       readFC fc
>     return ()

The final test is like the fourth, but with multiple consumer threads.

> test7 = forM [1..5] $ \i -> test7a i

> test7a size = do
>   beginTest $ "test7:"++(show size)
>   fc <- newFC size
>   forM [1..5] $ \_-> forkIO (consumer fc)
>   producer fc
>   endTest
>  where
>   counter = unsafePerformIO (newMVar 0)
>   producer fc = do
>     forM [1..10] $ \i-> do
>       threadDelay delayUnit
>       modifyMVar_ counter $ \c-> do
>         -- putStrLn $ (show c) ++ "<" ++ (show size)
>         assert (c==0) "queue should always be empty with slow writer"
>         return (c+1)
>       writeFC fc i
>     return ()
>   consumer fc = do
>     forM [1..rounds] $ \i-> do
>       j <- readFC fc
>       modifyMVar_ counter $ \c-> do
>          -- putStrLn $ (show c) ++ "<=" ++ (show size)
>          assert (c<=size) "queue size not within limit"
>          return (c-1)
>     return ()

> testFiniteChannel  = do
>   test1
>   test2
>   test3
>   test4
>   test5  
>   test6
>   test7



Problem 3: Type Inference
=========================

To solve this problem, you must first learn about how **type inference**
works in Haskell. First, look at [these notes](/lectures/lec-inference.html)
to understand how type inference works for a bare-bones language (basically
the lambda-calculus together with integers.)

Once you have understood that, you can start this problem, in which you
will add features to the bare-bones language, and update the inference to 
handle those features.

(a) Pairs
---------

The first feature we will add is pairs, ie tuples of size 2. 
Specifically, we have extended the language of expressions `Expr`
to include

~~~~~{.haskell}
data Exp = ...
         | Exp `ECom` Exp  -- Construct a pair of two expressions
         | EFst  Exp         -- Extract the first  element of a pair
	     | ESnd  Exp         -- Extract the second element of a pair
~~~~~

Correspondingly, we have extended the language of types to include

~~~~~{.haskell}
data Type = ... 
          |  Type `TCom` Type -- Pair of two types
~~~~~

Extend the definition of the `mgu` and `ti` functions to correctly infer
types for the extended language.



When you are done, you should be able to infer that the expression

> eSwap = EAbs (EV "x") $ (ELet (EV "a") (EFst (EVbl (EV "x")))
>                         (ELet (EV "b") (ESnd (EVbl (EV "x")))
>                         (EVbl (EV "b") `ECom` EVbl (EV "a"))))

has the type (equivalent to)

> tSwap = Forall [TV "a", TV "b"] $
>           (TVbl (TV "a") `TCom` TVbl (TV "b")) `TArr` ((TVbl (TV "b") `TCom` TVbl (TV "a")))


(b) Lists  
---------

Next, let us add lists, to the language. Specifically, we 
extend the language of expressions `Expr` to include

~~~~~{.haskell}
data Exp = ...
         | ENil            -- empty list
         | Exp `ECons` Exp -- head "cons-ed" to a tail
	     | EIsNil Exp      -- test if a list is empty
	     | EDcons Exp      -- return a pair of (head, tail) of (non-empty) list
~~~~~

Correspondingly, we have extended the language of types to include

~~~~~{.haskell}
data Type = ... 
          | TList Type     -- TList t is a list of t values 
~~~~~

Extend the definition of the `mgu` and `ti` functions to correctly infer
types for the extended language.

When you are done, you should be able to infer that in the environment 

> env   = Map.fromList 
>   [ (EV "plus", Forall [] $ tArrs [TInt, TInt, TInt])
>   , (EV "ite" , Forall [TV "a"] $ tArrs [TBool, TVbl (TV "a"), TVbl (TV "a"), TVbl (TV "a")])]
>
> tenv  = TypeEnv env


the expressions `eHd` and `eList`

> eZero = ELit (LInt 0)
> eOne  = ELit (LInt 1)
>
> ePlus = EVbl (EV "plus")
> eInc  = EAbs (EV "x") $ eApps [ePlus, eOne, EVbl (EV "x")]
> eHd   = EAbs (EV "x") $ EFst (EDcons (EVbl (EV "x")))
> eTl   = EAbs (EV "x") $ ESnd (EDcons (EVbl (EV "x")))
> eList = EAbs (EV "x") 
>           (eIf (EIsNil (EVbl (EV "x"))) 
>                eZero
>                (eInc `EApp` ((eHd `EApp` (EVbl (EV "x"))) `EApp` eZero)))


have the respective types

> tHd   = Forall [TV "a"] $ (TList (TVbl (TV "a"))) `TArr` (TVbl (TV "a"))
> tTl   = Forall [TV "a"] $ (TList (TVbl (TV "a"))) `TArr` (TList (TVbl (TV "a")))
> tList = Forall [] ((TList (TInt `TArr` TInt)) `TArr` TInt)

where the helper functions are defined

> tArrs = foldr1 TArr 
> eApps = foldl1 EApp
> eIf   = \b e1 e2 -> eApps [EVbl (EV "ite"), b, e1, e2] 



(c) Recursion 
-------------

Finally, we will add recursive functions to the language, via the following
construct.

~~~~~{.haskell}
data Exp = ...
         | ELetrec EVbl Exp Exp -- "ELetrec x e1 e2" allows x to appear in e1
~~~~~

We need not extend the language of types at all, and hence the `mgu`
function remains the same. However, the tricky bit is to figure out
how to break the following cycle: we need to use the type of x to
determine the type of e1, but we need to type e1 to determine the type
of x!


When you are done, you should be able to infer that the expressions `eTl`
and `eLen` 

>
> eLen = ERec (EV "len") 
>           (EAbs (EV "xs") $ 
>              eIf (EIsNil (EVbl (EV "xs"))) 
>                  eZero 
>                  (eInc `EApp`(((EVbl (EV "len")) `EApp` (eTl `EApp` (EVbl (EV "xs")))))))
>           (EVbl (EV "len"))
>
> eMap = ERec (EV "map") 
>           (EAbs (EV "f") $ EAbs (EV "xs") $ 
>              eIf (EIsNil (EVbl (EV "xs"))) 
>                  ENil
>                  (ECons (EVbl (EV "f") `EApp` (eHd `EApp` (EVbl (EV "xs"))))
>                         ((EVbl (EV "map") `EApp` (EVbl (EV "f"))) `EApp`
>                         (eTl `EApp` (EVbl (EV "xs"))))))
>
>           (EVbl (EV "map"))

have the types equivalent to

> tLen  = Forall [TV "a"] $ ((TList (TVbl (TV "a"))) `TArr` TInt)
> tMap  = Forall [TV "a", TV "b"] $ tArrs [TVbl (TV "a") `TArr` TVbl (TV "b")
>                                         ,TList (TVbl (TV "a"))
>                                         ,TList (TVbl (TV "b"))]


Appendix: Code for Type Inference from Lecture
----------------------------------------------

> data Exp     =  EVbl EVbl 
>              |  ELit Lit
>              |  EApp Exp Exp
>              |  EAbs EVbl Exp
>              |  ELet EVbl Exp Exp
>                                   -- part (a)
>              |  Exp `ECom` Exp    -- Construct a pair of two expressions
>              |  EFst  Exp         -- Extract the first  element of a pair
>	           |  ESnd  Exp         -- Extract the second element of a pair
>                                   -- part (b)
>              |  ENil              -- empty list
>              |  Exp `ECons` Exp   -- head "cons-ed" to a tail
>	           |  EIsNil Exp        -- test if a list is empty
>	           |  EDcons Exp        -- return a pair of (head, tail) of (non-empty) list
>                                   -- part (c)
>              |  ERec EVbl Exp Exp -- ERec x e1 e2 is like Let x e1 e2 but x can appear in e1
>              deriving (Eq, Ord)
>
> newtype EVbl = EV String deriving (Eq, Ord)
>
> data Lit     =  LInt Integer
>              |  LBool Bool
>              deriving (Eq, Ord)
>
> data Type    =  TVbl TVbl 
>              |  TInt
>              |  TBool
>              |  Type `TArr` Type
>                                    -- part (a)
>              |  Type `TCom` Type   -- Pair of two types
>                                    -- part (b)
>              |  TList Type         -- TList t is a list of t values 
>              deriving (Eq, Ord)
>
> newtype TVbl = TV String deriving (Eq, Ord)
>
> data Scheme  =  Forall [TVbl] Type
>
> newtype TypeEnv = TypeEnv (Map.Map EVbl Scheme)
>
> (\\) :: TypeEnv -> (EVbl, Scheme) -> TypeEnv
> (TypeEnv env) \\ (x, s) =  TypeEnv $ Map.insert x s env
>
> type Subst = Map.Map TVbl Type
>
> class Substitutable a where 
>   apply     :: Subst -> a -> a
>   freeTvars :: a -> Set.Set TVbl
>
> instance Substitutable Type where
>   apply _  TInt            = TInt
>   apply _  TBool           = TBool
>   apply su t@(TVbl a)      = Map.findWithDefault t a su 
>   apply su (t1 `TArr` t2)  = apply su t1 `TArr` apply su t2
>   -- apply su (t1 `TCom` t2)  = error "TBD"
>   apply su (t1 `TCom` t2)  = apply su t1 `TCom` apply su t2 -- error "TBD"
>   apply su (TList t)       = TList (apply su t)  
>
>   freeTvars TInt           =  Set.empty
>   freeTvars TBool          =  Set.empty
>   freeTvars (TVbl a)       =  Set.singleton a
>   freeTvars (t1 `TArr` t2) =  freeTvars t1 `Set.union` freeTvars t2
>   freeTvars (t1 `TCom` t2) =  freeTvars t1 `Set.union` freeTvars t2 -- error "TBD"
>   freeTvars (TList t)      =  (freeTvars t) --error "TBD"
>
> instance Substitutable Scheme where
>   apply s (Forall as t)   = Forall as $ apply s' t 
>                             where s' = foldr Map.delete s as 
>   freeTvars (Forall as t) = (freeTvars t) `Set.difference` (Set.fromList as)
>
>
> instance Substitutable a => Substitutable [a] where
>   apply     = map . apply
>   freeTvars = foldr Set.union Set.empty . map freeTvars
>
> instance Substitutable TypeEnv where
>   apply s   (TypeEnv env) =  TypeEnv   $ Map.map (apply s) env
>   freeTvars (TypeEnv env) =  freeTvars $ Map.elems env
>
> empSubst  ::  Subst
> empSubst  =   Map.empty
>
> after         :: Subst -> Subst -> Subst
> su1 `after` su2 = (Map.map (apply su1) su2) `Map.union` su1
>
>
> mgu (l `TCom` r) (l' `TCom` r')  = do  s1 <- mgu l l'
>                                        s2 <- mgu (apply s1 r) (apply s1 r')
>                                        return (s1 `after` s2)
> mgu (TList t1) (TList t2)        = mgu t1 t2 
> mgu (l `TArr` r) (l' `TArr` r')  = do  s1 <- mgu l l'
>                                        s2 <- mgu (apply s1 r) (apply s1 r')
>                                        return (s2 `after` s1)
> mgu (TVbl a) t                   = varAsgn a t
> mgu t (TVbl a)                   = varAsgn a t
> mgu TInt TInt                    = return empSubst
> mgu TBool TBool                  = return empSubst
> mgu t1 t2                        = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2
>
> varAsgn a t 
>   | t == TVbl a                  =  return empSubst
>   | a `Set.member` (freeTvars t) =  throwError $ "occur check fails: " ++ show a ++ " in " ++ show t
>   | otherwise                    =  return $ Map.singleton a t
>
> generalize        ::  TypeEnv -> Type -> Scheme
> generalize env t  =   Forall as t
>   where as = Set.toList $ (freeTvars t) `Set.difference` (freeTvars env)
>
> data TIState = TIState { count :: Int }
>
> fresh :: (MonadState TIState m) => m Int
> fresh = do s     <- get
>            let n = count s
>            put   $ s { count = n + 1 }
>            return n
>
> freshTVbl prefix = fresh >>= return . TVbl . TV . (prefix ++) . show
>
> instantiate (Forall as t) = do as' <- mapM (\ _ -> freshTVbl "a") as 
>                                let s = Map.fromList $ zip as as'
>                                return $ apply s t

> ti ::  (MonadState TIState m, MonadError String m) => 
>        TypeEnv -> Exp -> m (Subst, Type)
>
> ti env (ELit (LInt _))  = return (empSubst, TInt)
> ti env (ELit (LBool _)) = return (empSubst, TBool)
> ti (TypeEnv env) (EVbl x) = 
>     case Map.lookup x env of
>        Nothing   ->  throwError $ "unbound variable: " ++ show x
>        Just s    ->  instantiate s >>= return . (,) empSubst 
> ti env (EAbs x e) =
>     do  tv       <- freshTVbl "a"
>         let env' = env \\ (x, Forall [] tv)
>         (s1, t1) <- ti env' e
>         return (s1, (apply s1 tv) `TArr` t1)
> ti env (EApp e1 e2) =
>     do  tv       <- freshTVbl "a"
>         (s1, t1) <- ti env e1
>         (s2, t2) <- ti (apply s1 env) e2
>         s3       <- mgu (apply s2 t1) (TArr t2 tv)
>         return (s3 `after` s2 `after` s1, apply s3 tv)
> 
> ti env (ELet x e1 e2) =
>     do  (s1, t1) <- ti env e1
>         let t'   = generalize (apply s1 env) t1
>             env' = env \\ (x, t')
>         (s2, t2) <- ti (apply s1 env') e2
>         return (s2 `after` s1, t2)
> -- ti env (e1 `ECom` e2) = error "TBD"
> ti env (e1 `ECom` e2) = do (s1 , t1 ) <- ti env e1
>			     (s2 , t2 ) <- ti (apply s1 env) e2	
> --			     return ( s1 `after` s2, apply s2 t1 `TCom` t2)
>  			     return ( s1 `Map.union` s2, t1 `TCom` t2)
> -- ti env (EFst e)       = error "TBD"
> ti env (EFst e)       = do ta  <- freshTVbl "a" 
>			     tb  <- freshTVbl "b"
>			     (s1, t1) <- ti env e
>			     s2   <- mgu t1 ( ta `TCom` tb)
>			     return (s2 `after` s1 , apply s2 ta)
> -- ti env (ESnd e)       = error "TBD"
> ti env (ESnd e)       = do ta  <- freshTVbl "a" 
>			     tb  <- freshTVbl "b"
>			     (s1, t1) <- ti env e
>			     s2   <- mgu t1 ( ta `TCom` tb)
>			     return (s2 `after` s1 , apply s2 tb)
> -- ti env ENil            = error "TBD"
> ti env ENil            =  do ta <- freshTVbl "a"
>			       return (empSubst, TList ta)
> -- ti env (e1 `ECons` e2) = error "TBD"
> ti env (e1 `ECons` e2) = do (s1, t1 ) <- ti env e1
>			      (s2, t2 ) <- ti ( apply s1 env ) e2
>			      s3        <- mgu (TList (apply s2 t1 )) t2
> --			      return (s3 `after` s1, apply s3 t2)
> 			      return (s3 `after` s2 `after` s1, apply s3 t2)
> -- ti env (EIsNil e)      = error "TBD"
> ti env (EIsNil e)      = do ta <- freshTVbl "a"
>			      (s1, t1 ) <- ti env e
>			      s2   <- mgu t1 (TList ta)
>			     -- s2   <- mgu t1 (ta)
>			      return (s2 `after` s1, TBool )
> -- ti env (EDcons e)      = error "TDB"
> ti env (EDcons e)      = do ta  <- freshTVbl "a"
>			      (s1 , t1 ) <- ti env e
>			      s2 <- mgu t1 (TList ta)
>			      return (s2 `after` s1, apply s2 ta `TCom` apply s2 t1 )
> -- ti env (ERec x e1 e2)  = error "TBD"
> ti env (ERec x e1 e2)  = do ta <- freshTVbl "a"
>			      let ev = env \\ (x, Forall [] ta)
>			      (s1 , t1) <- ti ev e1
>			      (s2 , t2) <- ti (apply s1 ev) e2
>			      return (s2 `after` s1, t2)
>
> ti_top env e =
>     do  (s, t) <- ti env e
>         return  $ generalize (apply s env) (apply s t)
>
> typeInference :: TypeEnv -> Exp -> Either String Scheme 
> typeInference env e = res
>   where act = ti_top env e
>         res = evalState (runErrorT act) s0 
>         s0  = TIState { count = 0 }
>
>
> test :: Exp -> IO ()
> test e = case typeInference (TypeEnv Map.empty) e of
>            Left err  ->  putStrLn $ "error: " ++ err
>            Right t   ->  putStrLn $ show e ++ " :: " ++ show t


> instance Show TVbl where
>   showsPrec _ x = shows (prTVbl x)
>
> prTVbl (TV a) = PP.text a
>
> instance Show Type where
>   showsPrec _ x = shows (prType x)
> 
> prType             ::  Type -> PP.Doc
> prType (TVbl a)    =   prTVbl a
> prType TInt        =   PP.text "Int"
> prType TBool       =   PP.text "Bool"
> prType (TArr t s)  =   prParenType t PP.<+> PP.text "->" PP.<+> prType s
> prType (TCom t p)  =   PP.text "(" PP.<> prParenType t PP.<> PP.text "," PP.<+> prType p PP.<> PP.text ")"
> prType (TList t)   =   PP.text "[" PP.<> prParenType t PP.<> PP.text "]"
> -- prType _           =   PP.text "FINAL optional"
>
> prParenType     ::  Type -> PP.Doc
> prParenType  t  =   case t of
>                       TArr _ _  -> PP.parens (prType t)
>                       _         -> prType t
>
> instance Show EVbl where
>   showsPrec _ x = shows (prEVbl x)
>
> instance Show Exp where
>   showsPrec _ x = shows (prExp x)
>
> prEVbl (EV x)          = PP.text x
>
> prExp                  ::  Exp -> PP.Doc
> prExp (EVbl x)         =   prEVbl x
> prExp (ELit lit)       =   prLit lit
> prExp (ELet x b body)  =   PP.text "let" PP.<+> 
>                            prEVbl x PP.<+> PP.text "=" PP.<+>
>                            prExp b PP.<+> PP.text "in" PP.$$
>                            PP.nest 2 (prExp body)
> prExp (EApp e1 e2)     =   prExp e1 PP.<+> prParenExp e2
> prExp (EAbs x e)       =   PP.char '\\' PP.<+> prEVbl x PP.<+>
>                            PP.text "->" PP.<+>
>                            prExp e
> -- prExp _                =   PP.text "FINAL optional"
> prExp (e1 `ECom` e2)   =   PP.text "(" PP.<> prExp e1 PP.<> PP.text "," PP.<+> prExp e2 PP.<> PP.text ")"
> prExp (EFst e)         =   PP.text "fst" PP.<+> prExp e
> prExp (ESnd e)         =   PP.text "snd" PP.<+> prExp e
>
> prExp ENil             =   PP.text "[]"
> prExp (e `ECons` es)   =   PP.text "[" PP.<> prExp e PP.<> prCons es
>                              where prCons es = case es of
>                                                  ENil -> PP.text "]"
>                                                  x `ECons` xs -> PP.text "," PP.<> prExp x PP.<> prCons xs
>                                                  otherwise -> PP.text " Exp syntax error: The second argument of ECons is not a list"
> prExp (EIsNil e)       =   prParenExp e PP.<+> PP.text "==" PP.<+> prExp ENil
> prExp (EDcons e)       =   PP.text "(head" PP.<+> prParenExp e PP.<> PP.text ","
>                            PP.<+> PP.text "tail" PP.<+> prParenExp e PP.<> PP.text ")"
> prExp (ERec x e1 e2)   =   prExp (ELet x e1 e2)



>  
> prParenExp    ::  Exp -> PP.Doc
> prParenExp t  =   case t of
>                     ELet _ _ _  -> PP.parens (prExp t)
>                     EApp _ _    -> PP.parens (prExp t)
>                     EAbs _ _    -> PP.parens (prExp t)
>                     _           -> prExp t
> 
> instance Show Lit where
>     showsPrec _ x = shows (prLit x)
> 
> prLit            ::  Lit -> PP.Doc
> prLit (LInt i)   =   PP.integer i
> prLit (LBool b)  =   if b then PP.text "True" else PP.text "False"
> 
> instance Show Scheme where
>     showsPrec _ x = shows (prScheme x)
> 
> prScheme                ::  Scheme -> PP.Doc
> prScheme (Forall as t)  =   PP.text "All" PP.<+>
>                             PP.hcat (PP.punctuate PP.comma (map prTVbl as))
>                             PP.<> PP.text "." PP.<+> prType t

> 
>



Test cases: 

> e0  =  ELet (EV "id") (EAbs (EV "x") (EVbl (EV "x")))
>         (EVbl (EV "id"))
> 
> e1  =  ELet (EV "id") (EAbs (EV "x") (EVbl (EV "x")))
>         (EApp (EVbl (EV "id")) (EVbl (EV "id")))
> 
> e2  =  ELet (EV "id") (EAbs (EV "x") (ELet (EV "y") (EVbl (EV "x")) (EVbl (EV "y"))))
>         (EApp (EVbl (EV "id")) (EVbl (EV "id")))
> 
> e3  =  ELet (EV "id") (EAbs (EV "x") (ELet (EV "y") (EVbl (EV "x")) (EVbl (EV "y"))))
>         (EApp (EApp (EVbl (EV "id")) (EVbl (EV "id"))) (ELit (LInt 2)))
> 
> e4  =  ELet (EV "id") (EAbs (EV "x") (EApp (EVbl (EV "x")) (EVbl (EV "x"))))
>         (EVbl (EV "id"))
> 
> e5  =  EAbs (EV "m") (ELet (EV "y") (EVbl (EV "m"))
>                      (ELet (EV "x") (EApp (EVbl (EV "y")) (ELit (LBool True)))
>                           (EVbl (EV "x"))))
 
>


