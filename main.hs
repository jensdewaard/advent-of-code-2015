module Main where

import System.Environment (getArgs)
solve :: String -> IO ()
solve "1a" = do
      contents <- readFile "input/1"
      print $ sum $ map parseFloor contents
solve "1b" = do
      contents <- readFile "input/1"
      print $ findBasement (map parseFloor contents) 0 0
solve "2a" = do
  contents <- readFile "input/2"
  print $ sum $ map (wrappingPaper . parsePresent) $ lines contents
solve "2b" = do
  contents <- readFile "input/2"
  print $ sum $ map (ribbon . parsePresent) $ lines contents
solve _ = print "unknown puzzle"

main :: IO ()
main = do 
          args <- getArgs
          case length args of
            1 -> solve $ head args
            0 -> error "not enough arguments"
            _ -> error "too many arguments"

-- Day 1
parseFloor :: Char -> Int
parseFloor '(' = 1
parseFloor ')' = -1
parseFloor _ = 0

findBasement :: [Int] -> Int -> Int -> Int
findBasement [] f s = s
findBasement (x : xs) f s =
  if f < 0 then s else findBasement xs (f + x) (s + 1)

-- Day 2
data Present = Present Int Int Int

wrappingPaper :: Present -> Int
wrappingPaper (Present l h w) = 2 * l * w + 2 * w * h + 2 * h * l + min (l * w) (min (w * h) (h * l))

ribbon :: Present -> Int
ribbon (Present l h w) = l*h*w + 2*(l+h+w) - 2*max w (max h l)

parsePresent :: String -> Present
parsePresent s =
  let repl 'x' = ' '
      repl c = c
   in let (x : y : z : ns) = map (read :: String -> Int) $ words $ map repl s in Present x y z