{-# LANGUAGE OverloadedStrings #-}
-- Version 2022-05-19T18:11-0400

module Main where

import           Data.Text
import qualified Example.TreeTraverse as Ex1

main :: IO ()
main = do putStrLn "é"
          Ex1.main

