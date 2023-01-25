{-# LANGUAGE RankNTypes #-}

module ChurchList
    ( ChurchList(ChurchList)
    , fromList
    , repeat
    , filter
    ) where

import qualified Prelude as P
import Prelude hiding (repeat, filter, zip)
import Data.Foldable (toList)
import Control.Monad
import Control.Applicative

newtype ChurchList a = ChurchList { runList :: forall r. (a -> r -> r) -> r -> r }
instance Show a => Show (ChurchList a) where
    show = show . toList
instance Functor ChurchList where
    fmap f cl = ChurchList $ \k z -> runList cl (k . f) z
instance Applicative ChurchList where
    pure a = ChurchList $ \k z -> k a z
    cl1 <*> cl2 = ChurchList $ \k z -> runList cl1 (\g z' -> runList cl2 (k . g) z') z
instance Alternative ChurchList where
    empty = ChurchList $ flip const
    cl1 <|> cl2 = ChurchList $ \k z -> runList cl1 k (runList cl2 k z)
instance Monad ChurchList where
    cl >>= f = ChurchList $ \k z -> runList cl (\x z' -> runList (f x) k z') z
instance MonadPlus ChurchList where
    mzero = empty
    mplus = (<|>)
instance Foldable ChurchList where
    foldr f z cl = runList cl f z
instance Traversable ChurchList where
    traverse f cl = runList cl (\x rest -> cons <$> f x <*> rest) (pure mempty)
instance Semigroup (ChurchList a) where
    (<>) = mplus
instance Monoid (ChurchList a) where
    mempty = empty

fromList :: [a] -> ChurchList a
fromList cl = ChurchList $ \k z -> foldr k z cl

cons :: a -> ChurchList a -> ChurchList a
cons x cl = ChurchList $ \k z -> k x (runList cl k z)


repeat :: a -> ChurchList a
repeat x = cons x $ repeat x

filter :: (a -> Bool) -> ChurchList a -> ChurchList a
filter = mfilter