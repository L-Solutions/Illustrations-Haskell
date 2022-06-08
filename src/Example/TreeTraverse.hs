{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Example.UtilisationMTrans
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-05-19T18:16-0400
--
-- Exemple de l'impact et de l'importance des transformateur de monades
--
-----------------------------------------------------------------------------

module Example.TreeTraverse
    where

import           Control.Applicative
import           Control.Monad
import           Data.Tree

t0 = Node ['1','2'] []
t1 = Node ['a','b'] []
t2 = Node ['x'] []

t3 :: Tree String
t3 = Node "R" [t0, Node "N" [t1, t2]]

t4 = Node '1' [ Node '2' [], Node '3' [] ]
t5 = Node 'a' [ Node 'b' [ Node 'c' [] ]]

printT :: Show a => Tree a -> IO ()
printT = putStrLn . drawTree . fmap show

printF :: Show a => Forest a -> IO ()
printF = mapM_ printT

treeTraverse :: Applicative f => (t -> f a) -> Tree t -> f (Tree a)
treeTraverse g (Node x ts) = Node <$> g x <*> forestTraverse g ts

forestTraverse :: Applicative f => (t -> f a) -> Forest t -> f (Forest a)
forestTraverse g ts = sequenceA $ map (treeTraverse g) ts

main :: IO ()
main = do putStrLn "============= Example.UtilisationMTrans ============="
          putStrLn "--------------------------------------- Premier exemple"
          printF $ treeTraverse (++"0") $ Node "R" [t1]
          putStrLn "--------------------------------------- Deuxième exemple"
          printF $ sequence t3
          putStrLn "--------------------------------------- Troisième exemple"
          printT $ sequence [t4, t5]
          putStrLn "--------------------------------------- Quatrième exemple"
          printT $ sequence [t5, t4]

