module Main where

import Day1 ( parseFloor, findBasement )
import Day2 ( wrappingPaper, ribbon, parsePresent )
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


-- Day 2