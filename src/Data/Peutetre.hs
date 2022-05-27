-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Peutetre
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-05-27T09:22-0400
--
-----------------------------------------------------------------------------

module Data.Peutetre
    where

data Peutetre a = Rien | Juste a
    deriving (Show, Eq, Read)

fromList :: [a] -> Peutetre a
fromList []    = Rien
fromList (x:_) = Juste x

fromMaybe :: Maybe a -> Peutetre a
fromMaybe Nothing  = Rien
fromMaybe (Just x) = Juste x

instance Semigroup (Peutetre a) where
    -- (<>) :: Peutetre a -> Peutetre a -> Peutetre a
    -- Associativity
    --   (a <> b) <> c == a <> (b <> c)
    j@Juste{} <> _  = j
    Rien      <> pe = pe

instance Monoid a => Monoid (Peutetre a) where
    -- mempty :: Peutetre a
    -- Right identity
    --   x <> mempty = x
    -- Left identity
    --   mempty <> x = x
    mempty = Rien

instance Functor Peutetre where
    -- fmap :: (a -> b) -> Peutetre a -> Peutetre b
    -- Identity
    --   fmap id == id
    -- Composition
    --   fmap (f . g) == fmap f . fmap g
    fmap f Rien      = Rien
    fmap f (Juste x) = Juste $ f x

{-
    Monoid m
    mempty  :: m
    mappend :: m -> m -> m
-}
instance Foldable Peutetre where
    -- foldMap :: Monoid m => (a -> m) -> Peutetre a -> m
    -- Composition of fold and map
    -- with fold = foldMap id
    --   foldMap f = fold . fmap f
    -- this means
    --   foldMap f . fmap g = foldMap (f . g)
    -- Specifically for Monoid (Peutetre a)
    --   foldMap id ls = fold ls = ls
    foldMap _ Rien      = mempty
    foldMap f (Juste x) = f x

{-
    Applicative f
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}
instance Traversable Peutetre where
    -- traverse :: Applicative f => (a -> f b) -> Peutetre a -> f (Peutetre b)
    -- Naturality
    --   t . traverse f = traverse (t . f)
    --   where t is an applicative transformation
    --     t :: (Applicative f, Applicative g) => f a -> g a
    -- Identity
    --   traverse Identity = Identity
    -- Composition
    --   traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
    traverse _ Rien      = pure Rien
    traverse f (Juste x) = Juste <$> f x

instance Applicative Peutetre where
    -- pure  :: a -> Peutetre a
    pure x = undefined
    -- (<*>) :: Peutetre (a -> b) -> Peutetre a -> Peutetre b
    -- Identity
    --   pure id <*> v = v
    -- Composition
    --   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- Homomorphism
    --   pure f <*> pure x = pure (f x)
    -- Interchange
    --   u <*> pure y = pure ($ y) <*> u
    (<*>) Rien _       = Rien
    (<*>) (Juste f) pe = fmap f pe

instance Monad Peutetre where
    -- return :: a -> Peutetre a
    return = undefined
    -- (>>=)  :: Peutetre a -> (a -> Peutetre b) -> Peutetre b
    -- Left identity
    --   return a >>= k = k a
    -- Right identity
    --   m >>= return = m
    -- Associativity
    --   m >>= (\x -> k x >>= h) = (m >>= k) >>= h
    (>>=) Rien _      = Rien
    (>>=) (Juste x) k = k x


