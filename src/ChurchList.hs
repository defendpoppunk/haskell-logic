{-# LANGUAGE RankNTypes #-}

module ChurchList
    ( ChurchList(ChurchList)
    , fromList
    ) where

import Data.Foldable (toList)

newtype ChurchList a = ChurchList { runList :: forall r. (a -> r -> r) -> r -> r }
instance Show a => Show (ChurchList a) where
    show = ("CL" ++) . show . toList
instance Functor ChurchList where
    fmap f xs = ChurchList $ \k z -> runList xs (\x xs' -> k (f x) xs') z
instance Applicative ChurchList where
    pure = fromList . pure                             -- UPDATE!!!!
    xs <*> ys = fromList $ (toList xs) <*> (toList ys) -- UPDATE!!!!
instance Foldable ChurchList where
    foldr f z xs = runList xs f z
instance Traversable ChurchList where
    traverse f xs = runList xs (\x rest -> cons <$> f x <*> rest) (pure mempty)
instance Semigroup (ChurchList a) where
    xs <> ys = ChurchList $ \k z -> runList xs k (runList ys k z)
instance Monoid (ChurchList a) where
    mempty = ChurchList $ flip const

fromList :: [a] -> ChurchList a
fromList xs = ChurchList $ \k z -> foldr k z xs

cons :: a -> ChurchList a -> ChurchList a
cons x xs = ChurchList $ \k z -> k x (runList xs k z)