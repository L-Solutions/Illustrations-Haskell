module Data.Soit
    where

data Soit r a = Gauche r | Droite a
    deriving (Show, Eq, Read)

fromList :: Monoid m => [a] -> Gauche m a
fromList []    = Gauche mempty
fromList (x:_) = Droite x

fromEither :: Either r a -> Soit r a
fromEither (Left x)  = Gauche x
fromEither (Right x) = Droite x

instance Semigroup (Soit r a) where
    -- (<>) :: Soit r a -> Soit r a -> Soit r a
    -- Associativity
    --   (a <> b) <> c == a <> (b <> c)
    (<>) = undefined

instance Monoid (Soit r a) where
    -- mempty :: Soit r a
    -- Right identity
    --   x <> mempty = x
    -- Left identity
    --   mempty <> x = x
    mempty = undefined

instance Functor (Soit r) where
    -- fmap :: (a -> b) -> Soit r a -> Soit r b
    -- Identity
    --   fmap id == id
    -- Composition
    --   fmap (f . g) == fmap f . fmap g
    fmap f pe = undefined

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
    foldMap f pe = undefined

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
    traverse f pe = undefined

instance Applicative (Soit r) where
    -- pure  :: a -> Soit r a
    pure x = undefined
    -- (<*>) :: Soit r (a -> b) -> Soit r a -> Soit r b
    -- Identity
    --   pure id <*> v = v
    -- Composition
    --   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- Homomorphism
    --   pure f <*> pure x = pure (f x)
    -- Interchange
    --   u <*> pure y = pure ($ y) <*> u
    (<*>) = undefined

instance Monad (Soit r) where
    -- return :: a -> Soit r a
    return = undefined
    -- (>>=)  :: Soit r a -> (a -> Soit r b) -> Soit r b
    -- Left identity
    --   return a >>= k = k a
    -- Right identity
    --   m >>= return = m
    -- Associativity
    --   m >>= (\x -> k x >>= h) = (m >>= k) >>= h
    (>>=) = undefined


