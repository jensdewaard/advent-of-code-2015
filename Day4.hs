#!/usr/bin/env nix-shell
#!nix-shell --pure -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [ pkgs.turtle pkgs.pureMD5 ])"

module Day4 where

import qualified Data.Digest.Pure.MD5 as D
import Data.List
import Data.String
import qualified Data.ByteString as B

main :: IO ()
main = do
    let key = "ckczppom"
    let m = find (\l -> "00000" `isPrefixOf` hash l) [key ++ show x | x <- [1..10000000]]
    let m' = find (\l -> "000000" `isPrefixOf` hash l) [key ++ show x | x <- [1..10000000]]
    --- 490464 is too high
    print m
    print m'

hash :: String -> String
hash = show . D.md5 . fromString
