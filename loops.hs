import System.Random

getHugeNumber :: IO ()
getHugeNumber = do  {
                        putStr "Enter a big number: ";
                        x <- getLine;
                        if (read x) > 1000 then do  {
                            putStr "Big number: ";
                            putStrLn x
                                                    }
                        else    do {
                            putStrLn "Can you think of a bigger one?";
                            getHugeNumber
                                }
                    }

printStarsInLine :: Int -> IO()
printStarsInLine 0 = putStrLn ""
printStarsInLine n = do {
                    putChar '*';
                    printStarsInLine (n-1)
                        }

getRandomNumberfrom1to10000 :: IO Int
getRandomNumberfrom1to10000 = randomRIO (1,10000)

printStars :: Int -> Int -> IO()
printStars x n | n == x = printStarsInLine x
               | otherwise = do {
                            printStarsInLine x;
                            printStars (x + 1) n
                                }

