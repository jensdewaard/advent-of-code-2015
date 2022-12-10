#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Day3 where

import Data.List

data Move = U | D | L | R deriving Show

parseMove :: Char -> Move
parseMove '^' = U
parseMove 'v' = D
parseMove '<' = L
parseMove '>' = R

doMove :: (Int,Int) -> Move -> (Int, Int)
doMove (x, y) U = (x, y+1)
doMove (x, y) R = (x+1, y)
doMove (x, y) L = (x-1, y)
doMove (x, y) D = (x, y-1)

main :: IO ()
main = do
    contents <- readFile "input/3"
    let ms = indexed $ map parseMove contents
    let ls = scanl doMove (0,0) (map snd ms)
    let santa = [m | (i, m) <- ms, even i]
    let robo = [m | (i, m) <- ms, odd i]
    let m = [(p, o) | p <- nub ls, let o = count p ls]
    let lsS = scanl doMove (0,0) santa
    let lsR = scanl doMove (0,0) robo
    let ls' = lsS ++ lsR
    let m' = [(p, o) | p <- nub ls', let o = count p ls']
    print $ length m
    print $ length m'

count :: Eq a => a -> [a] -> Int
count n h = length $ filter (==n) h

indexed :: [a] -> [(Int, a)]
indexed [] = []
indexed l = indexed' 0 l where
    indexed' _ [] = []
    indexed' n (x : xs) = (n, x) : indexed' (n + 1) xs


