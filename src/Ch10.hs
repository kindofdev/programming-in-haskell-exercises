module Ch10 where
import System.IO

-- ex1

putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]


-- ex2

type Board = [Int]

initial :: Board
initial = [7,6,5,4,3,2,1]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr $ show row
                    putStr ": "
                    putStrLn $ concat $ replicate num "* "

putBoard :: Board -> IO ()
putBoard b = putBoard' b 1

putBoard' :: Board -> Int -> IO ()
putBoard' []     _ = return ()
putBoard' (x:xs) r = do putRow r x
                        putBoard' xs (r+1)



-- ex3

putBoard'' :: Board -> IO ()
putBoard'' b = sequence_ [putRow r n | (n,r) <- zip b [1..]]



-- ex4

adder :: IO ()
adder = do putStr "How many numbers? "
           n     <- getNumber
           total <- readNumbers 0 n
           putStrLn $ "The total is " ++ show total

readNumbers :: Int -> Int -> IO Int
readNumbers total 0      = return total
readNumbers total remain = do n <- getNumber
                              readNumbers (total + n) (remain -1)

getNumber :: IO Int
getNumber = read <$> getLine

-- ex5

adder' :: IO ()
adder' = do putStr "How many numbers? "
            n     <- getNumber
            total <- sum <$> sequence (replicate n getNumber)
            putStrLn $ "The total is " ++ show total



-- ex6

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

getLine' :: IO String
getLine' = getLine'' ""


getLine'' :: String -> IO String
getLine'' xs = do x <- getCh
                  case x of
                    '\n'   -> do putChar '\n'
                                 return $ reverse xs
                    '\DEL' -> if null xs then
                                getLine'' ""
                              else
                                do putStr "\b \b"
                                   getLine'' (tail xs)
                    _      -> do putChar x
                                 getLine'' (x:xs)
