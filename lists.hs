createPalindrome :: [Char] -> [Char]
createPalindrome xs = xs ++ reverse xs

calculateDistance :: Floating a => (a, a) -> (a, a) -> a
calculateDistance (x1,y1) (x2,y2) = sqrt((x1 - x2)**2 + (y1 - y2)**2)

safetail1 :: [a] -> [a]
safetail1 xs = if length xs == 0 then [] else tail xs

safetail2 :: Eq a => [a] -> [a]
safetail2 xs | xs == [] = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

squareNumbers :: Int -> [Int]
squareNumbers n = [x*x | x <-[1..n]]

task2 :: [Int]
task2 = [x | x <- [22..85], x `mod` 11 /= 0]

numberpals :: [Int]
numberpals = [x | x <- [17..1012], show x == reverse (show x)]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

leastfactor :: Int -> Int
leastfactor num = (factors num)!!1

pairs :: [(Int,Int)]
pairs = [(x,leastfactor x) | x <- [2..100]]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

pythagorean :: Int -> [(Int,Int,Int)]
pythagorean n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x+y*y==z*z]

