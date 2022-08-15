-----------------------------------------------------------------------------
-- |
-- Module      :  Example.Finance
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-08-11T11:14-0400
--
-----------------------------------------------------------------------------

module Example.Finance
    where

intérêt montant taux mois = montant * (1 + taux / 100)**(mois/12) - montant

solde m t n r = let i = (1 + t / 100)**(1/12)
                in (m * i**n) - r * (1-i**n) / (1-i)

main :: IO ()
main = putStrLn "Finance"

