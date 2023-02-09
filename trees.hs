
surfaceToCube :: Float -> Float
surfaceToCube x = (sqrt x)**3

-- interactive programs can be written using IO functions
-- these are special functions that can have side effects
-- that is why they are noted as IO, as haskell does not have side effects
-- we can seperate IO function and others in testing

reverseInput :: IO()
reverseInput = do   {
                    putStr "Enter a string: ";
                    xs <- getLine;
                    putStrLn (reverse xs)
                    }


countLarge :: [Int] -> Int
countLarge [] = 0
countLarge (x:xs) = if x > 100 then 1 + (countLarge xs) else (countLarge xs)

countLarge1 :: [Int] -> Int
countLarge1 xs = sum [1 | x <- xs, x > 100]

countLarge2 :: [Int] -> Int
countLarge2 xs = length (filter (\ x -> x>100) xs)

countLarge3 :: [Int] -> Int
countLarge3 xs = sum (map (\ x -> if x>100 then 1 else 0) xs)


-- Ord a => (a,a) -> Bool
increasingPair (x,y) = x < y

-- Num a => a -> a -> a
g x y = 2*x + y

-- [(Bool -> Bool -> Bool)]
sth = [(&&),(||)]

data Tree = Leaf | Node Tree Int Tree

flatten :: Tree -> [Int]
flatten Leaf = []
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

ex1 = Node (Node (Node Leaf 1 Leaf) 3 (Node Leaf 4 Leaf)) 5 (Node Leaf 7 (Node Leaf 9 Leaf))

ex2 = Node (Node (Node Leaf 4 Leaf) 3 (Node Leaf 4 Leaf)) 5 (Node Leaf 7 (Node Leaf 9 Leaf))

--insert :: Int -> Tree -> Tree
--insert n (Node l x r) = if n