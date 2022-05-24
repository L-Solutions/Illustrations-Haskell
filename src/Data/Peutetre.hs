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
    -- (<>) :: Liste a -> Liste a -> Liste a
    -- Associativity
    --   (a <> b) <> c == a <> (b <> c)
    (<>) = undefined

instance Monoid (Peutetre a) where
    -- mempty :: Liste a
    -- Right identity
    --   x <> mempty = x
    -- Left identity
    --   mempty <> x = x
    mempty = undefined

instance Functor Peutetre where
    -- fmap :: (a -> b) -> Liste a -> Liste b
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
instance Foldable Peutetre where
    -- foldMap :: Monoid m => (a -> m) -> Liste a -> m
    -- Composition of fold and map
    -- with fold = foldMap id
    --   foldMap f = fold . fmap f
    -- this means
    --   foldMap f . fmap g = foldMap (f . g)
    -- Specifically for Monoid (Liste a)
    --   foldMap id ls = fold ls = ls
    foldMap f pe = undefined

{-
    Applicative f
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}
instance Traversable Peutetre where
    -- traverse :: Applicative f => (a -> f b) -> Liste a -> f (Liste b)
    -- Naturality
    --   t . traverse f = traverse (t . f)
    --   where t is an applicative transformation
    --     t :: (Applicative f, Applicative g) => f a -> g a
    -- Identity
    --   traverse Identity = Identity
    -- Composition
    --   traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
    traverse f pe = undefined

instance Applicative Peutetre where
    -- pure  :: a -> Liste a
    pure x = undefined
    -- (<*>) :: Liste (a -> b) -> Liste a -> Liste b
    -- Identity
    --   pure id <*> v = v
    -- Composition
    --   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- Homomorphism
    --   pure f <*> pure x = pure (f x)
    -- Interchange
    --   u <*> pure y = pure ($ y) <*> u
    (<*>) = undefined

instance Monad Peutetre where
    -- return :: a -> Liste a
    return = undefined
    -- (>>=)  :: Liste a -> (a -> Liste b) -> Liste b
    -- Left identity
    --   return a >>= k = k a
    -- Right identity
    --   m >>= return = m
    -- Associativity
    --   m >>= (\x -> k x >>= h) = (m >>= k) >>= h
    (>>=) = undefined


