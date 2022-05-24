module Data.Liste
    where

data Liste a = Vide | Ajoute a (Liste a)
    deriving (Show, Eq, Read)

fromList :: [a] -> Liste a
fromList []     = Vide
fromList (x:xs) = Ajoute x $ fromList xs

instance Semigroup (Liste a) where
    -- (<>) :: Liste a -> Liste a -> Liste a
    -- Associativity
    --   (a <> b) <> c == a <> (b <> c)
    Vide        <> ls  = ls
    Ajoute l ls <> ls' = Ajoute l (ls <> ls')

instance Monoid (Liste a) where
    -- mempty :: Liste a
    -- Right identity
    --   x <> mempty = x
    -- Left identity
    --   mempty <> x = x
    mempty = Vide

instance Functor Liste where
    -- fmap :: (a -> b) -> Liste a -> Liste b
    -- Identity
    --   fmap id == id
    -- Composition
    --   fmap (f . g) == fmap f . fmap g
    fmap f Vide          = Vide
    fmap f (Ajoute e es) = Ajoute (f e) $ fmap f es

{-
    Monoid m
    mempty  :: m
    mappend :: m -> m -> m
-}
instance Foldable Liste where
    -- foldMap :: Monoid m => (a -> m) -> Liste a -> m
    -- Composition of fold and map
    -- with fold = foldMap id
    --   foldMap f = fold . fmap f
    -- this means
    --   foldMap f . fmap g = foldMap (f . g)
    -- Specifically for Monoid (Liste a)
    --   foldMap id ls = fold ls = ls
    foldMap f Vide          = mempty
    foldMap f (Ajoute e es) = f e `mappend` foldMap f es

{-
    Applicative f
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}
instance Traversable Liste where
    -- traverse :: Applicative f => (a -> f b) -> Liste a -> f (Liste b)
    -- Naturality
    --   t . traverse f = traverse (t . f)
    --   where t is an applicative transformation
    --     t :: (Applicative f, Applicative g) => f a -> g a
    -- Identity
    --   traverse Identity = Identity
    -- Composition
    --   traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
    traverse f Vide          = pure Vide
    traverse f (Ajoute e es) = pure Ajoute <*> f e <*> traverse f es

instance Applicative Liste where
    -- pure  :: a -> Liste a
    pure x = Ajoute x Vide
    -- (<*>) :: Liste (a -> b) -> Liste a -> Liste b
    -- Identity
    --   pure id <*> v = v
    -- Composition
    --   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    -- Homomorphism
    --   pure f <*> pure x = pure (f x)
    -- Interchange
    --   u <*> pure y = pure ($ y) <*> u
    Vide        <*> _           = Vide
    _           <*> Vide        = Vide
    Ajoute f fs <*> Ajoute e es = Ajoute (f e) (fmap f es)
                                  <> (fs <*> pure e)
                                  <> (fs <*> es)

instance Monad Liste where
    -- return :: a -> Liste a
    return = pure
    -- (>>=)  :: Liste a -> (a -> Liste b) -> Liste b
    -- Left identity
    --   return a >>= k = k a
    -- Right identity
    --   m >>= return = m
    -- Associativity
    --   m >>= (\x -> k x >>= h) = (m >>= k) >>= h
    Vide        >>= _ = Vide
    Ajoute e es >>= k = k e <> (es >>= k)

