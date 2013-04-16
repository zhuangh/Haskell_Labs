
data Shape = Rectangle Side Side
            | Ellipse Radius Radius
            | RtTriangle Side Side
            | Polygon [Vertex]
            deriving Show
 
type Radius = Float 
type Side   = Float
type Vertex = (Float, Float)




rectangle :: Side -> Side -> Shape
rectangle l w = Polygon [ (0.0,0.0), (l, 0.0), ( l, w), (0.0 ,w)]
-- rectangle l w = Polygon [ (l/2.0, w/2.0), (l/2.0, -w/2.0), (-l/2.0,-w/2.0) ,(-l/2.0, w/2.0)]
-- rectangle = error "Define me!" 


rtTriangle :: Side -> Side -> Shape
rtTriangle l w = Polygon [ (0.0, 0.0 ), (l, 0.0), (0.0, w) ] 
--rtTriangle = error "Define me!" 
polySides :: Vertex -> Vertex -> Vertex -> Int
polySides v1 v2 v3 = 3 

sides :: Shape -> Int
--sides (RtTriangle l w )		= 3
--sides (Rectangle l w )		= 4
sides (Ellipse	r1 r2)		= 42
sides (RtTriangle l w )		= sides (rtTriangle l w)
sides (Rectangle l w )		= sides (rectangle l w)

sides (Polygon	(v1:v2))	= 0 
sides (Polygon	(v1:v2:v3:vs))  = 3 + length (vs) 
sides (Polygon	(v1:v2:v3))	= 3 
sides (Polygon	(v1))	= 0

-- sides (Polygon  _)		= 0
-- sides ([Vertex]   _)		= 0
--    where   polySides  :: [Vertex]  -> Int
--	    polySides (v2:v3:vs)    =  + polySides (v3) 
--	    polySides (_)       = 0
-- sides (_) = 0
-- sides = error "Define me!"

-- gbigger :: Vertex -> Float -> Vertex
-- gbigger (x, y) ts = (x*ts, y *ts)   

bigger :: Shape -> Float -> Shape
bigger Polygon ([(v1x,v1y)])  = Polygon [(v1x*sqrt(2),v1y*sqrt(2) )]
-- bigger (Polygon v1 ) ts = gbigger v1 ts 
--				bigger vs ts 



-- Part 2

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 0 _ _ _ = return ( )
hanoi n a b c = do 
		hanoi (n-1) a c b
		putStrLn("move disk from " ++a ++ " to "++ b)
		hanoi (n-1) c b a 
{--
hfunc :: Int -> a -> a -> a -> [(a,a)] 
hfunc 0 _ _ _ = [ ]
hfunc n a b c = hfunc (n-1) a c b ++ [(a,b)] ++ hfunc (n-1) c b a
 -- hfunc n a b c 
		-- putStrLn("move disc from " ++ a ++ " to " ++ b) 
--}
-- Part 3


