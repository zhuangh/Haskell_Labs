!!! For your own good, do not read my crappy code when you are taking this courses. 


---
title: Homework #1, Due Friday, January 20th
---

Preliminaries
-------------

Before starting this assignment:

1. Read chapters 1 -- 3 of The Haskell School of Expression.
2. Download and install the [Glasgow Haskell Compiler (GHC)](http://www.haskell.org/ghc/).
3. Download the SOE code bundle from
   [the Haskell School of Expression page](/static/SOE.tar.gz).
4. Verify that it works by changing into the `SOE/src` directory and
   running `ghci Draw.lhs`, then typing `main0` at the prompt:
 
~~~
cd SOE/src
ghci Draw.lhs
*Draw> main0
~~~

  You should see a window with some shapes in it.

**NOTE:** If you have trouble installing SOE, [see this page](soe-instructions.html)

5. Download the required files for this assignment: [hw1.tar.gz](/static/hw1.tar.gz).
   Unpack the files and make sure that you can successfully run the main program (in `Main.hs`).
   We've provided a `Makefile`, which you can use if you like. You should see this output:

~~~
Main: Define me!
~~~

Haskell Formalities
-------------------

We declare that this is the Hw1 module and import some libraries:

> module Hw1 where
> import SOE
> import Play
> import XMLTypes

Part 1: Defining and Manipulating Shapes
----------------------------------------

You will write all of your code in the `hw1.lhs` file, in the spaces
indicated. Do not alter the type annotations --- your code must
typecheck with these types to be accepted.

The following are the definitions of shapes from Chapter 2 of SOE:

> data Shape = Rectangle Side Side
>            | Ellipse Radius Radius
>            | RtTriangle Side Side
>            | Polygon [Vertex]
>            deriving Show
> 
> type Radius = Float 
> type Side   = Float
> type Vertex = (Float, Float)

1. Below, define functions `rectangle` and `rtTriangle` as suggested
   at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
   built with the Polygon constructor.

> rectangle :: Side -> Side -> Shape
> rectangle l b = Polygon[(0, 0), (l, 0), (l, b), (0, b)]

> rtTriangle :: Side -> Side -> Shape
> rtTriangle b h = Polygon[(0, 0), (b, 0), (0, h)]

2. Define a function

> sides :: Shape -> Int
> sides (Rectangle _ _) = 4
> sides (Ellipse _ _) = 42
> sides (RtTriangle _ _) = 3
> sides (Polygon []) = 0
> sides (Polygon (_:[])) = 0
> sides (Polygon (_:_:[])) = 0
> sides (Polygon (_:_:_:[])) = 3
> sides (Polygon (_:v1:v2:vs)) = 1 + sides (Polygon (v1:v2:vs))

  which returns the number of sides a given shape has.
  For the purposes of this exercise, an ellipse has 42 sides,
  and empty polygons, single points, and lines have zero sides.

3. Define a function

> mulVertex :: Float -> Vertex -> Vertex
> mulVertex m (p1, p2) = (p1 * m, p2 * m)
> bigger :: Shape -> Float -> Shape
> bigger (Rectangle l b) e = (Rectangle (l * (sqrt e)) (b * (sqrt e)))
> bigger (Ellipse a b) e = (Ellipse (a * (sqrt e)) (b * (sqrt e)))
> bigger (RtTriangle b h) e = (RtTriangle (b * (sqrt e)) (h * (sqrt e)))
> bigger (Polygon vlist) e = Polygon (map (mulVertex (sqrt e)) vlist)

  that takes a shape `s` and expansion factor `e` and returns
  a shape which is the same as (i.e., similar to in the geometric sense)
  `s` but whose area is `e` times the area of `s`.

4. The Towers of Hanoi is a puzzle where you are given three pegs,
   on one of which are stacked $n$ discs in increasing order of size.
   to another by moving only one disc at a time and never stacking
   a larger disc on top of a smaller one.
   
   To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:
   
   1. Move $n - 1$ discs from peg $a$ to peg $c$.
   2. Move the remaining disc from peg $a$ to peg $b$.
   3. Move $n - 1$ discs from peg $c$ to peg $b$.
   
   Write a function
   
> hanoi :: Int -> String -> String -> String -> IO ()
> hanoi 0 _ _ _ = return ( )
> hanoi n a b c = do
>       hanoi (n-1) a c b 
>       putStrLn("move disk from " ++a ++ " to "++ b)
>       hanoi (n-1) c b a 

  that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
  where a is the starting peg,
  emits the series of moves required to solve the puzzle.
  For example, running `hanoi 2 "a" "b" "c"`

  should emit the text

~~~  
move disc from a to c
move disc from a to b
move disc from c to b
~~~

Part 2: Drawing Fractals
------------------------

1. The Sierpinski Carpet is a recursive figure with a structure similar to
   the Sierpinski Triangle discussed in Chapter 3:

![Sierpinski Carpet](/static/scarpet.png)

Write a function `sierpinskiCarpet` that displays this figure on the
screen:

> minSize :: Int
> minSize = 6

> fillCpt :: Window -> Int -> Int -> Int -> IO ()
> fillCpt w x y size = drawInWindow w (withColor Blue
>                                     (polygon [(x, y), (x + size, y), (x + size, y - size), (x, y - size), (x, y)]))
> sierpinskiCpt :: Window -> Int -> Int -> Int -> IO ()
> sierpinskiCpt w x y size 
>                     = if size <= minSize
>                       then fillCpt w x y size
>                       else let size2 = size `div` 3
>                       in do sierpinskiCpt w x y size2
>                             sierpinskiCpt w (x+size2) y size2
>                             sierpinskiCpt w (x+2*size2) y size2
>                             sierpinskiCpt w x (y-size2) size2
>                             sierpinskiCpt w (x+2*size2) (y-size2) size2
>                             sierpinskiCpt w x (y-2*size2) size2
>                             sierpinskiCpt w (x+size2) (y-2*size2) size2
>                             sierpinskiCpt w (x+2*size2) (y-2*size2) size2

> sierpinskiCarpet :: IO ()
> sierpinskiCarpet = runGraphics $ do  
>                      w <- openWindow "The Sierpinski Carpet" (400, 400)
>                      sierpinskiCpt w 50 300 243
>                      k <- getKeyChar w
>                      putStrLn $ "You pressed " ++ show k
>                      closeWindow w

Note that you either need to run your program in `SOE/src` or add this
path to GHC's search path via `-i/path/to/SOE/src/`.
Also, the organization of SOE has changed a bit, so that now you use
`import SOE` instead of `import SOEGraphics`.

2. Write a function `myFractal` which draws a fractal pattern of your
   own design.  Be creative!  The only constraint is that it shows some
   pattern of recursive self-similarity.

> fillShape :: Window -> Int -> Int -> Int -> IO ()
> fillShape w x y size = drawInWindow w (withColor Yellow
>                                       (polygon [(x, y), (x + size, y), (x + size, y - size), (x, y - size), (x, y)]))
> myFractalDraw :: Window -> Int -> Int -> Int -> IO ()
> myFractalDraw w x y size 
>                     = if size <= minSize
>                       then fillShape w x y size
>                       else let size2 = size `div` 3
>                       in do myFractalDraw w x y size2
>                             myFractalDraw w (x+2*size2) y size2
>                             myFractalDraw w (x+size2) (y-size2) size2
>                             myFractalDraw w x (y-2*size2) size2
>                             myFractalDraw w (x+2*size2) (y-2*size2) size2

> myFractal :: IO ()
> myFractal = runGraphics $ do  
>                      w <- openWindow "My Fractal" (400, 400)
>                      myFractalDraw w 50 300 243
>                      k <- getKeyChar w
>                      putStrLn $ "You pressed " ++ show k
>                      closeWindow w

Part 3: Transforming XML Documents
----------------------------------

First, a warmup:

1. Read chapters 5 and 7 of SOE.

2. Do problems 5.3, 5.5, 5.6, 7.1, and 7.2 from SOE, and turn them
   is as part of the source code you create below.

   Your `maxList` and `minList` functions may assume that the lists
   they are passed contain at least one element.

> lengthNonRecrusive :: [a] -> Int
> lengthNonRecrusive = foldl (\n _ -> 1 + n) 0

  Alternatively: lengthNonRecrusive = foldr (\_ n -> 1 + n) 0
-------------------------------------------------------------------

> doubleEach :: [Int] -> [Int]
> doubleEach [] = []
> doubleEach (x : xs) = (2 * x) : (doubleEach xs)

  I assume this is supposed to be a recursive implementation.
-------------------------------------------------------------------

> doubleEachNonRecursive :: [Int] -> [Int]
> doubleEachNonRecursive = foldr (\x xs -> (2*) x : xs) []

  Alternatively: doubleEach = map (2*)
  If this is considered as non-recursive
-------------------------------------------------------------------

> pairAndOne :: [Int] -> [(Int, Int)]
> pairAndOne [] = []
> pairAndOne (x : xs) = (x, x + 1) : (pairAndOne xs)

  I assume this is supposed to be a recursive implementation.
-------------------------------------------------------------------

> pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
> pairAndOneNonRecursive = foldr (\x xs -> (x, x + 1) : xs) []

  Alternatively: pairAndOneNonRecursive = map (\x -> (x, x + 1))
  If this is considered as non-recursive
-------------------------------------------------------------------

> addEachPair :: [(Int, Int)] -> [Int]
> addEachPair [] = []
> addEachPair ((x, y) : xs) = (x + y) : (addEachPair xs)

  I assume this is supposed to be a recursive implementation.
-------------------------------------------------------------------

> addEachPairNonRecursive :: [(Int, Int)] -> [Int]
> addEachPairNonRecursive = foldr (\(x, y) xs -> (x + y) : xs) []

  Alternatively: addEachPairNonRecursive = map (\(x, y) -> (x + y))
  If this is considered as non-recursive
-------------------------------------------------------------------

> minList :: [Int] -> Int
> minList [] = error "Empty list input is not allowed!"
> minList [x] = x
> minList (x:xs) = min x (minList xs)

  I assume this is supposed to be a recursive implementation.
-------------------------------------------------------------------

> minListNonRecursive :: [Int] -> Int
> minListNonRecursive l = foldr (\x y -> min x y) (head l) l

  Alternatively: minListNonRecursive l = foldl (\x y -> min x y) (head l) l
-------------------------------------------------------------------

> maxList :: [Int] -> Int
> maxList [] = error "Empty list input is not allowed!"
> maxList [x] = x
> maxList (x:xs) = max x (maxList xs)

  I assume this is supposed to be a recursive implementation.
-------------------------------------------------------------------

> maxListNonRecursive :: [Int] -> Int
> maxListNonRecursive l = foldr (\x y -> max x y) (head l) l

  Alternatively: maxListNonRecursive l = foldl (\x y -> max x y) (head l) l
-------------------------------------------------------------------

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>               deriving (Show, Eq)

> treeFold :: (t1 -> t1 -> t1) -> (t -> t1) -> Tree t -> t1
> treeFold _ b (Leaf x) = b x
> treeFold op b (Branch l r) = (treeFold op b l) `op` (treeFold op b r)

> fringe :: Tree a -> [a]
> fringe (Leaf x) = [x]
> fringe (Branch n1 n2) = fringe n1 ++ fringe n2

> treeSize :: Tree a -> Int
> treeSize (Leaf _) = 1
> treeSize (Branch n1 n2) = treeSize n1 + treeSize n2

> treeHeight :: Tree a -> Int
> treeHeight (Leaf _) = 0
> treeHeight (Branch n1 n2) = 1 + max (treeHeight n1) (treeHeight n2)

> data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
>                       deriving (Show, Eq)

> takeTree :: Int -> InternalTree a -> InternalTree a
> takeTree _ ILeaf = ILeaf
> takeTree n (IBranch a tl tr) = if n == 0 then ILeaf
>                           else if n < 0 then error "Wrong level number input!"
>                           else IBranch a (takeTree (n - 1) tl) (takeTree (n - 1) tr)  

> takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
> takeTreeWhile _ ILeaf = ILeaf
> takeTreeWhile f (IBranch a tl tr) = if (f a) then IBranch a (takeTreeWhile f tl) (takeTreeWhile f tr)
>                                     else ILeaf

Write the function map in terms of foldr:

> myMap :: (a -> b) -> [a] -> [b]
> myMap f = foldr (\x xs -> (f x) : xs) []

The rest of this assignment involves transforming XML documents.
To keep things simple, we will not deal with the full generality of XML,
or with issues of parsing. Instead, we will represent XML documents as
instances of the following simpliﬁed type:

~~~~
data SimpleXML =
   PCDATA String
 | Element ElementName [SimpleXML]
 deriving Show

type ElementName = String
~~~~

That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
data") node containing a string or else an `Element` node containing a
tag and a list of sub-nodes.

The file `Play.hs` contains a sample XML value. To avoid getting into
details of parsing actual XML concrete syntax, we'll work with just
this one value for purposes of this assignment. The XML value in
`Play.hs` has the following structure (in standard XML syntax):

~~~
<PLAY>
  <TITLE>TITLE OF THE PLAY</TITLE>
  <PERSONAE>
    <PERSONA> PERSON1 </PERSONA>
    <PERSONA> PERSON2 </PERSONA>
    ... -- MORE PERSONAE
    </PERSONAE>
  <ACT>
    <TITLE>TITLE OF FIRST ACT</TITLE>
    <SCENE>
      <TITLE>TITLE OF FIRST SCENE</TITLE>
      <SPEECH>
        <SPEAKER> PERSON1 </SPEAKER>
        <LINE>LINE1</LINE>
        <LINE>LINE2</LINE>
        ... -- MORE LINES
      </SPEECH>
      ... -- MORE SPEECHES
    </SCENE>
    ... -- MORE SCENES
  </ACT>
  ... -- MORE ACTS
</PLAY>
~~~

* `sample.html` contains a (very basic) HTML rendition of the same
  information as `Play.hs`. You may want to have a look at it in your
  favorite browser.  The HTML in `sample.html` has the following structure
  (with whitespace added for readability):
  
~~~
<html>
  <body>
    <h1>TITLE OF THE PLAY</h1>
    <h2>Dramatis Personae</h2>
    PERSON1<br/>
    PERSON2<br/>
    ...
    <h2>TITLE OF THE FIRST ACT</h2>
    <h3>TITLE OF THE FIRST SCENE</h3>
    <b>PERSON1</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
    <b>PERSON2</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
    <h3>TITLE OF THE SECOND SCENE</h3>
    <b>PERSON3</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
  </body>
</html>
~~~

You will write a function `formatPlay` that converts an XML structure
representing a play to another XML structure that, when printed,
yields the HTML speciﬁed above (but with no whitespace except what's
in the textual data in the original XML).

> formatPERS :: SimpleXML -> String 
> formatPERS (PCDATA s) = s
> formatPERS (Element _ [PCDATA s]) = s++"<br/>" 
> formatPERS _ = error "Wrong input format!"

> formatSPEECH :: SimpleXML -> String
> formatSPEECH (PCDATA s) = s
> formatSPEECH (Element "SPEAKER" [PCDATA s]) = "<b>"++s++"</b><br/>"
> formatSPEECH (Element "LINE" [PCDATA s]) = s++"<br/>"
> formatSPEECH _ = error "Wrong input format!"

> formatSCENE :: SimpleXML -> String 
> formatSCENE (PCDATA s) = s
> formatSCENE (Element "TITLE" body) = "<h3>"++ (concat (map formatSCENE body)) ++"</h3>"
> formatSCENE (Element "SPEECH" body) = concat (map formatSPEECH body)
> formatSCENE _ = error "Wrong input format!"

> formatACT :: SimpleXML -> String 
> formatACT (PCDATA s) = s
> formatACT (Element "TITLE" body) = "<h2>"++ (concat (map formatACT body)) ++"</h2>" 
> formatACT (Element "SCENE" body) = concat (map formatSCENE body)
> formatACT _ = error "Wrong input format!"

> formatPlayInPlay :: SimpleXML -> SimpleXML 
> formatPlayInPlay (PCDATA s) = (PCDATA s)
> formatPlayInPlay (Element tag []) = Element tag []
> formatPlayInPlay (Element "TITLE" body) = Element "h1" body
> formatPlayInPlay (Element "PERSONAE" body) = PCDATA ("<h2>Dramatis Personae</h2>" ++ (concat (map formatPERS body)) )
> formatPlayInPlay (Element "ACT" body) = PCDATA (concat (map formatACT body) )
> formatPlayInPlay _ = error "Wrong input format!"

> formatPlay :: SimpleXML -> SimpleXML
> formatPlay (PCDATA s) = (PCDATA s) 
> formatPlay (Element tag []) = Element tag []
> formatPlay (Element "PLAY" body) = Element "html" [Element "body" (map formatPlayInPlay body)]
> formatPlay _ = error "Wrong input format!"

The main action that we've provided below will use your function to
generate a ﬁle `dream.html` from the sample play. The contents of this
ﬁle after your program runs must be character-for-character identical
to `sample.html`.

> mainXML :: IO ()
> mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
>              testResults "dream.html" "sample.html"
>
> firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
> firstDiff [] [] = Nothing
> firstDiff (c:cs) (d:ds) 
>      | c==d = firstDiff cs ds 
>      | otherwise = Just (c:cs, d:ds)
> firstDiff cs ds = Just (cs,ds)
> 
> testResults :: String -> String -> IO ()
> testResults file1 file2 = do 
>   f1 <- readFile file1
>   f2 <- readFile file2
>   case firstDiff f1 f2 of
>     Nothing -> do
>       putStr "Success!\n"
>     Just (cs,ds) -> do
>       putStr "Results differ: '"
>       putStr (take 20 cs)
>       putStr "' vs '"
>       putStr (take 20 ds)
>       putStr "'\n"

Important: The purpose of this assignment is not just to "get the job
done" --- i.e., to produce the right HTML. A more important goal is to
think about what is a good way to do this job, and jobs like it. To
this end, your solution should be organized into two parts:

1. a collection of generic functions for transforming XML structures
   that have nothing to do with plays, plus

2. a short piece of code (a single deﬁnition or a collection of short
   deﬁnitions) that uses the generic functions to do the particular
   job of transforming a play into HTML.

Obviously, there are many ways to do the ﬁrst part. The main challenge
of the assignment is to ﬁnd a clean design that matches the needs of
the second part.

You will be graded not only on correctness (producing the required
output), but also on the elegance of your solution and the clarity and
readability of your code and documentation.  Style counts.  It is
strongly recommended that you rewrite this part of the assignment a
couple of times: get something working, then step back and see if
there is anything you can abstract out or generalize, rewrite it, then
leave it alone for a few hours or overnight and rewrite it again. Try
to use some of the higher-order programming techniques we've been
discussing in class.

Submission Instructions
-----------------------

* If working with a partner, you should both submit your assignments
  individually.
* Make sure your `hw1.lhs` is accepted by GHC without errors or warnings.
* Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
  subject "HW1" (minus the quotes).
  *This address is unmonitored!*

Credits
-------

This homework is essentially Homeworks 1 & 2 from
<a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
