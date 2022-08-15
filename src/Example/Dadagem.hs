{-# LANGUAGE DerivingStrategies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Example.Dadagem
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-08-11T13:54-0400
--
-----------------------------------------------------------------------------

module Example.Dadagem
    where

import           Data.Map (Map (..))
import qualified Data.Map as M
import           Data.Set (Set (..))
import qualified Data.Set as S


-- isomophism between Set (a b)' <-> 'Map a (Set b)' <-> 'Set (a, Set b))'
--
-- 'Set (a b)' is isomorphic to 'Map a (Set b)' via 'fromSet' and 'toSet'
-- and 'Map a (Set b)' is 'Set (a, Set b))' via 'fromList'/'toList'
--
fromSet :: (Ord a, Ord b) => Set (a, b) -> Map a (Set b)
fromSet s = S.foldr go M.empty s
    where go :: (Ord a, Ord b) => (a, b) -> Map a (Set b) -> Map a (Set b)
          go (x,y) = M.alter (add y) x
          add :: Ord b => b -> Maybe (Set b) -> Maybe (Set b)
          add y Nothing  = Just $ S.singleton y
          add y (Just s) = Just $ S.insert y s


toSet :: (Ord a, Ord b) => Map a (Set b) ->  Set (a,b)
toSet m = S.unions $ map (uncurry f) $ M.toList m
    where f :: (Ord a, Ord b) => a -> Set b -> Set (a,b)
          f x y = S.map ((,) x) y

-- data set
s1 = S.fromList [(x, y) | x <- [1 .. 2], y <- ['a' .. 'c']] `add` (3, 'd') `add` (4, 'a')
    where add = flip S.insert

