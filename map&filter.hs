import Data.Char

--twice :: (a -> a) -> a -> a
twice f x = f (f x)

isSpace :: Char -> Bool
isSpace x | x == ' ' = True
          | otherwise = False

calc10 :: (Int -> a) -> a
calc10 f = f 10

extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs) = if isDigit x then extractDigits xs else x : extractDigits xs

extractDigits1 :: String -> String
extractDigits1 xs = filter (not . isDigit) xs

extractDigits2 :: String -> String
extractDigits2 xs = [x | x <- xs, (not . isDigit) x]

palindrome :: [Char] -> Bool
palindrome xs = reverse xs == xs

square :: Int -> Int
square x = x * x

testlist :: [[Char]]
testlist =["madam", "adam", "otto", "elsa", "kajak", "tomas"]

-- transform to non-higher-order functions
test0 = map (+3) [2,3,4,5,6]

test0b = [x + 3 | x <- [2,3,4,5,6]]


test1 = map palindrome testlist

test1b = [palindrome x | x <- testlist]


test2 = filter palindrome testlist

test2b = [x | x <- testlist, palindrome x]


test3 = [(square x) + 3| x <-[1..500]]

test3b = map (+3) (map square [1..500])

test3c = map (\ x -> square x + 3) [1..500]


challenge = [x|x <-testlist, length x >2]

challengeb = filter (\x -> length x > 2) testlist


