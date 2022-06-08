-----------------------------------------------------------------------------
-- |
-- Module      :  Example.Download
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-06-02T15:17-0400
--
-----------------------------------------------------------------------------

module Example.Download
    where

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit.Combinators     (sinkFile)
import           Network.HTTP.Conduit         (parseRequest)
import           Network.HTTP.Simple          (httpSink)

downloadFile :: String -> IO ()
downloadFile url = do
  request <- parseRequest url
  runResourceT $ httpSink request $ \_ -> sinkFile "tmpfile"


