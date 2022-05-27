-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Soit
-- Copyright   :  Benoît Fraikin 2022
-- License     :  BSD3
--
-- Maintainer  :  benoit.fraikin@usherbrooke.ca
-- Stability   :  experimental
-- Portability :  portable
-- Version     :  2022-05-26T14:29-0400
--
-----------------------------------------------------------------------------

module Data.Soit
    where

data Soit r a = Gauche r | Droite a
    deriving (Show, Eq, Read)

fromEither :: Either r a -> Soit r a
fromEither (Left x)  = Gauche x
fromEither (Right x) = Droite x

instance Semigroup (Soit r a) where
    -- (<>) :: Soit r a -> Soit r a -> Soit r a
    -- Associativity
    --   (a <> b) <> c == a <> (b <> c)
    d@Droite{} <> _ = d
    Gauche _   <> s = s

{-
instance Monoid a => Monoid (Soit r a) where
    -- mempty :: Soit r a
    -- Right identity
    --   x <> mempty = x
    -- Left identity
    --   mempty <> x = x
    mempty = Droite mempty
-}

instance Functor (Soit r) where
    -- fmap :: (a -> b) -> Soit r a -> Soit r b
    -- Identity
    --   fmap id == id
    -- Composition
    --   fmap (f . g) == fmap f . fmap g
    fmap f (Droite x) = Droite $ f x
    fmap _ (Gauche y) = Gauche y

{-
    Monoid m
    mempty  :: m
    mappend :: m -> m -> m
-}
instance Foldable (Soit r) where
    -- foldMap :: Monoid m => (a -> m) -> Soit r a -> m
    -- Composition of fold and map
    -- with fold = foldMap id
    --   foldMap f = fold . fmap f
    -- this means
    --   foldMap f . fmap g = foldMap (f . g)
    -- Specifically for Monoid (Soit r a)
    --   foldMap id ls = fold ls = ls
    foldMap f (Droite x) = f x
    foldMap _ _          = mempty

{-
    Applicative f
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}
instance Traversable (Soit r) where
    -- traverse :: Applicative f => (a -> f b) -> Soit r a -> f (Soit r b)
    -- Naturality
    --   t . traverse f = traverse (t . f)
    --   where t is an applicative transformation
    --     t :: (Applicative f, Applicative g) => f a -> g a
    -- Identity
    --   traverse Identity = Identity
    -- Composition
    --   traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
    traverse f (Droite x) = pure Droite <*> f x
    traverse _ (Gauche y) = pure $ Gauche y

instance Applicative (Soit r) where
    -- pure  :: a -> Soit r a
    pure = Droite
    -- (<*>) :: Soit r (a -> b) -> Soit r a -> Soit r b
    -- Identity
    --   pure id <*> v = v
    -- Composition
    --   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- Homomorphism
    --   pure f <*> pure x = pure (f x)
    -- Interchange
    --   u <*> pure y = pure ($ y) <*> u
    Droite f <*> d = fmap f d
    Gauche x <*> _ = Gauche x

instance Monad (Soit r) where
    -- return :: a -> Soit r a
    return = pure
    -- (>>=)  :: Soit r a -> (a -> Soit r b) -> Soit r b
    -- Left identity
    --   return a >>= k = k a
    -- Right identity
    --   m >>= return = m
    -- Associativity
    --   m >>= (\x -> k x >>= h) = (m >>= k) >>= h
    Droite x >>= k = k x
    Gauche x >>= _ = Gauche x


