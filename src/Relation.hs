module Relation
    ( Relation'(Rel)
    , Relation
    , relationToMatrix
    , matrixToRelation
    , filter
    ) where

import qualified Prelude as P
import Prelude hiding (filter)
import RelationItem (RelationItem)
import qualified RelationItem as RI
import Data.Foldable (toList)


data Relation' a = Rel [a] deriving (Show, Eq)
instance Foldable Relation' where
    foldr f x (Rel xs) = foldr f x xs
instance Functor Relation' where
    fmap f (Rel xs) = Rel $ fmap f xs

type Relation a = Relation' (RelationItem a)


filter :: (a -> Bool) -> Relation' a -> Relation' a
filter p r = Rel $ P.filter p (toList r)


matrixToRelation :: [[a]] -> Relation a
matrixToRelation xs = Rel $ reverse $ map (RI.fromList) xs

relationToMatrix :: Relation a -> [[a]]
relationToMatrix (Rel xs) = reverse $ map toList xs