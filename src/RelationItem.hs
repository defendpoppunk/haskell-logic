module RelationItem
    ( RelationItem(RelItem)
    , fromList
    , filter
    , zip
    , sortBy
    , zipWith
    , relationItemsComposable
    , composeRelationItems
    , truncateRelationItemByMask
    ) where

import qualified Prelude as P
import Prelude hiding (filter, zip, zipWith)
import Data.Foldable (toList)
import qualified Data.List as L


data RelationItem a = RelItem [a] deriving (Show)
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

zipWith :: (a -> b -> c) -> RelationItem a -> RelationItem b -> RelationItem c
zipWith f ri1 ri2 = RelItem $ P.zipWith f (toList ri1) (toList ri2)


relationItemsComposable :: Eq a => RelationItem a -> RelationItem a -> Bool
relationItemsComposable (RelItem [])  _          = True
relationItemsComposable  _           (RelItem []) = True
relationItemsComposable (RelItem xs) (RelItem ys) = (last xs) == (head ys)

composeRelationItems :: RelationItem a -> RelationItem a -> RelationItem a
composeRelationItems  ri          (RelItem []) = ri
composeRelationItems (RelItem [])  ri          = ri
composeRelationItems (RelItem xs) (RelItem ys) = RelItem $ (init xs) ++ (tail ys)

truncateRelationItemByMask :: RelationItem a -> RelationItem Bool -> RelationItem a
truncateRelationItemByMask ri vm = fmap fst $ filter snd $ zip ri vm