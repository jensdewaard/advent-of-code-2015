#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle ])"

module Day1 where

-- Day 1
parseFloor :: Char -> Int
parseFloor '(' = 1
parseFloor ')' = -1
parseFloor _ = 0

findBasement :: [Int] -> Int -> Int -> Int
findBasement [] f s = s
findBasement (x : xs) f s =
  if f < 0 then s else findBasement xs (f + x) (s + 1)
