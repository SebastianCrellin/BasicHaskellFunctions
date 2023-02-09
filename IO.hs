printZ :: IO ()
printZ = putChar 'z'

strlen :: IO()
strlen = do {
                putStr "Enter a string: ";
                xs <- getLine;
                putStr "The string has ";
                putStr (show (length xs));
                putStrLn " characters."
            }

diff      :: String -> String -> String
diff xs ys =
   [if elem x ys then x else '-' | x <- xs]

-- actual lab session

data Direction = North | East | South | West deriving Show

turnAround :: Direction -> Direction
turnAround North = South
turnAround South = North
turnAround West = East
turnAround East = West

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

safetailMaybe :: [a] -> Maybe [a]
safetailMaybe [] = Nothing
safetailMaybe (x:xs) = Just xs

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe i xs | i > length xs = Nothing
               | otherwise = Just (take i xs)


data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)

ex1 = Unary (Unary (Unary (Unary (Unary (Unary (Unary (Leaf 0) 1) 2) 3) 4) 5) 6) 7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Binary (Leaf 4) 5 (Leaf 6))
ex3 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Binary (Unary (Leaf 3) 4) 5 (Leaf 6))

depth :: Btree a -> Int
depth (Leaf x) = 0
depth (Unary l x) = 1 + depth l
depth (Binary l x r) = 1 + (max (depth l) (depth r))

btreeToList :: Btree a -> [a]
btreeToList (Leaf x) = [x]
btreeToList (Unary l x) = btreeToList l ++ [x]
btreeToList (Binary l x r) = btreeToList l ++ x : btreeToList r


mapBtree :: (a -> b) -> Btree a -> Btree b
mapBtree f (Leaf x) = Leaf (f x)
mapBtree f (Unary l x) = Unary (mapBtree f l) (f x)
mapBtree f (Binary l x r) = Binary (mapBtree f l) (f x) (mapBtree f r)

-- optional questions

type Pos = (Int,Int)

followDirection :: Pos -> Direction -> Pos
followDirection (x,y) North = (x,y+1)
followDirection (x,y) East = (x+1,y)
followDirection (x,y) South = (x,y-1)
followDirection (x,y) West = (x-1,y)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing f = Nothing
bindMaybe (Just x) f = f x

takeMaybe 