module RelationItem
    ( RelationItem(RelItem)
    , fromList
    , filter
    , zip
    , sortBy
    ) where

import qualified Prelude as P
import Prelude hiding (filter, zip)
import Data.Foldable (toList)
import qualified Data.List as L


data RelationItem a = RelItem [a] deriving (Show, Eq)
instance Foldable RelationItem where
    foldr f x (RelItem xs) = foldr f x xs
instance Functor RelationItem where
    fmap f (RelItem xs) = RelItem $ fmap f xs


fromList :: [a] -> RelationItem a
fromList = RelItem

filter :: (a -> Bool) -> RelationItem a -> RelationItem a
filter p ri = RelItem $ P.filter p (toList ri)

zip :: RelationItem a -> RelationItem b -> RelationItem (a, b)
zip ri1 ri2 = RelItem $ P.zip (toList ri1) (toList ri2)

sortBy :: (a -> a -> Ordering) -> RelationItem a -> RelationItem a
sortBy f ri = RelItem $ L.sortBy f (toList ri)