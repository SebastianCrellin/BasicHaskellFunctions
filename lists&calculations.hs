-- 2014114 - 4028228
doubleMe :: Int -> Int
doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = 2*x + 2*y

doubleUs1 :: Int -> Int -> Int
doubleUs1 x y = doubleMe x + doubleMe y

quadrupleMe :: Int -> Int
quadrupleMe x = doubleMe (doubleMe x)

third :: [Int] -> Int
third xs = xs!!2

last2 :: [Int] -> Int
last2 xs = xs!!(length xs - 1)

init2 :: [Int] -> [Int]
init2 xs = take (length xs - 1) xs

last3 :: [Int] -> Int
last3 xs = head (reverse xs)

init3 :: [Int] -> [Int]
init3 xs = reverse (tail (reverse xs))
