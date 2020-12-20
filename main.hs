module Main where

import System.Environment ( getArgs )

factorial :: (Eq p, Num p) => p -> p
factorial n = if n == 0 then 1 else n * factorial (n - 1)

parseFloor :: Char -> Int
parseFloor '(' = 1
parseFloor ')' = -1
parseFloor _ = 0

findBasement :: [Int] -> Int -> Int -> Int
findBasement [] f s = s
findBasement (x:xs) f s =
    if f < 0 then s else findBasement xs (f + x) (s + 1)


solve :: String -> IO ()
solve "1a" = do
       contents <- readFile "input/1"
       print $ sum $ map parseFloor contents
solve "1b" = do
        contents <- readFile "input/1"
        print $ findBasement (map parseFloor contents) 0 0
solve _ = print "unknown puzzle"

main :: IO ()
main = do 
          args <- getArgs
          case length args of
            1 -> solve $ head args
            0 -> error "not enough arguments"
            _ -> error "too many arguments"


