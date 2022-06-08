{-# LANGUAGE OverloadedStrings #-}
-- Version 2022-06-08T15:39-0400

module Main where

import           Data.Text                               (Text (..))
import           Data.Text.Utils
import qualified Example.Download                        as Ex1 (main)
import qualified Example.TreeTraverse                    as Ex2 (main)
import qualified Example.TwoMonadTransformerForSameMonad as Ex3 (main)

entête :: Text
entête =  "#######"                                                         <> eol
       <> "#        #    #  ######  #    #  #####   #       ######   ####"  <> eol
       <> "#         #  #   #       ##  ##  #    #  #       #       #"      <> eol
       <> "#####      ##    #####   # ## #  #    #  #       #####    ####"  <> eol
       <> "#          ##    #       #    #  #####   #       #            #" <> eol
       <> "#         #  #   #       #    #  #       #       #       #    #" <> eol
       <> "#######  #    #  ######  #    #  #       ######  ######   ####"  <> eol

liste :: [ IO () ]
liste =  [ Ex1.main, Ex2.main, Ex3.main]

main :: IO ()
main = do putText entête
          putText eol
          entier <- getLine
          let n = read entier
              numéro = n - 1
          putTextLn $ "Exemple : " <> pack (show numéro)
          --
          liste !! numéro
          --
          putTextLn "Fin."

