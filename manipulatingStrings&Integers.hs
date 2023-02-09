--(’a’,’b’,’c’) -> (char, char, char) GOOD
--[’a’,’b’,’c’] -> [char] GOOD
--[(’1’, False), (’0’,True)] -> [(char, Bool)] GOOD
--([True,False], [’0’,’1’]) -> ([Bool], [char]) GOOD

areEqual :: Eq a => [a] -> [a] -> Bool
areEqual as bs = as == bs

areEqualAt ::  Eq a => [a] -> [a] -> Int -> Bool
-- areEqualAt as bs n = as!!n == bs!!n
areEqualAt as bs n = if n < length as && n < length bs then as!!n == bs!!n else False

third :: (a,b,c) -> c -- GOOD
third (x,y,z) = z

swap :: (a,b) -> (b,a) -- GOOD
swap (x,y) = (y,x)

addsquares :: (Num a) => (a,a) -> a
addsquares (x, y) = x * x + y * y

ordered :: Ord a => a -> a -> a-> Bool
ordered x y z = x<=y && y<=z

palinedrome :: Eq a => [a] -> Bool
palinedrome xs = (xs == reverse xs)

-- second :: [a] -> a
second xs = head (tail xs)


-- pair b -> c -> (b,c)
pair x y      = (x,y)
-- double :: Num a => a -> a
double x      = x*2
-- palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs
-- twice 
twice f x= f (f x)

safetail :: Eq a => [a] -> [a]
safetail xs | xs == []  = []
            | otherwise = tail xs

or1 :: Bool -> Bool -> Bool
or1 a b = if a == True || b == True then True else False

or2 :: Bool -> Bool -> Bool
or2 a b | a == True || b == True = True
        | otherwise = False

or3 :: Bool -> Bool -> Bool
or3 True _ = True
or3 _ True = True
or3 _ _ = False
