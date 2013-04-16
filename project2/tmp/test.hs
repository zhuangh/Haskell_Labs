

data Tree a = Leaf a
	    | Branch (Tree a) (Tree a) 
	    deriving (Show, Eq)

class Functor f where
    fmap :: (a->b) -> f a -> f b


instance Functor Tree where
    fmap f (Leaf x )      = Leaf (f x)
    fmap f (Branch t1 t2) = Branch (fmap f t1) (fmap f t2)


myTree = Branch 'd'  
