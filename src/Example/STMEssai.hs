-----------------------------------------------------------------------------
-- |
-- Module      :  Example.STMEssai
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-08-11T11:15-0400
--
-----------------------------------------------------------------------------

module Example.STMEssai
    where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad          (forever, replicateM_)
import           Say

makeCounter :: TVar Int -> IO (IO Int)
makeCounter var = do
  return $ atomically $ do value <- readTVar var
                           writeTVar var (1 + value)
                           return value

main1 :: IO ()
main1 = do
  var <- newTVarIO 1
  counter <- makeCounter var
  replicateM_ 10 $ counter >>= sayShow

main2 :: IO ()
main2 = do
  aliceVar <- newTVarIO 0
  bobVar <- newTVarIO 0

  _ <- forkIO $ payAlice aliceVar

  atomically $ do
    currentAlice <- readTVar aliceVar
    check $ currentAlice > 20
    writeTVar aliceVar (currentAlice - 20)
    currentBob <- readTVar bobVar
    writeTVar bobVar (currentBob + 20)

  finalAlice <- atomically $ readTVar aliceVar
  finalBob <- atomically $ readTVar bobVar

  sayString $ "Final Alice: " ++ show finalAlice
  sayString $ "Final Bob: " ++ show finalBob

payAlice :: TVar Int -> IO ()
payAlice aliceVar = replicateM_ 10 $ do
  threadDelay 1000000
  atomically $ do
    current <- readTVar aliceVar
    writeTVar aliceVar (current + 5)
  sayString "Paid Alice"

