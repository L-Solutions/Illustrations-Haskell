-----------------------------------------------------------------------------
-- |
-- Module      :  Example.TwoMonadTransformerForSameMonad
-- Copyright   :  Daniel Wagner (galois.com) 2022
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-06-02T15:17-0400
--
-----------------------------------------------------------------------------

module Example.TwoMonadTransformerForSameMonad where

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans

-- | laws (from [1]):
-- swap . fmap (fmap f) = fmap (fmap f) . swap
-- swap . pure = fmap pure
-- swap . fmap pure = pure
-- fmap join . swap . fmap (join . fmap swap) = join . fmap swap . fmap join . swap
class Monad m => Swap m where
    swap :: m (m a) -> m (m a)

instance Swap Identity where swap = id
instance Swap Maybe where
    swap Nothing         = Just Nothing
    swap (Just Nothing)  = Nothing
    swap (Just (Just x)) = Just (Just x)

newtype Twice m a = Twice { runTwice :: m (m a) }

instance Functor m => Functor (Twice m) where
    fmap f = Twice . fmap (fmap f) . runTwice

instance Applicative m => Applicative (Twice m) where
    pure    = Twice . pure . pure
    f <*> x = Twice $ (<*>) <$> runTwice f <*> runTwice x

instance Swap m => Monad (Twice m) where
    (>>=) x k = j $ fmap k x

j :: (Monad m, Swap m) => Twice m (Twice m a) -> Twice m a
j = Twice                        -- rewrap newtype
  . fmap join . join . fmap swap -- from [1]
  . runTwice . fmap runTwice     -- unwrap newtype

instance MonadTrans Twice where lift = Twice . pure

