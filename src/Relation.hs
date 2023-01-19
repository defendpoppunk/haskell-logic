module Relation
    ( Relation'(Rel)
    , Relation
    , relationToMatrix
    , matrixToRelation
    , filter
    , combineRelation
    , composeRelation
    , relationHasResults
    , permuteRelation
    ) where

import Prelude hiding (filter)
import qualified Prelude as P
import RelationItem hiding (filter, zip, sortBy, zipWith)
import qualified RelationItem as RI
import Data.Foldable (toList)
import Control.Applicative


data Relation' a = Rel [a] deriving (Show, Eq)
instance Foldable Relation' where
    foldr f x (Rel xs) = foldr f x xs
instance Functor Relation' where
    fmap f (Rel xs) = Rel $ fmap f xs
instance Semigroup (Relation' a) where
    (Rel xs) <> (Rel ys) = Rel $ xs <> ys
instance Applicative Relation' where
    pure = Rel . pure
    (Rel xs) <*> (Rel ys) = Rel $ xs <*> ys

type Relation a = Relation' (RelationItem a)


filter :: (a -> Bool) -> Relation' a -> Relation' a
filter p r = Rel $ P.filter p (toList r)


combineRelation :: Relation' a -> Relation' a -> Relation' a
combineRelation = (<>)

composeRelation :: Eq a => Relation a -> Relation a -> Relation a
composeRelation r1 r2 = fmap (uncurry composeRelationItems) 
                        $ filter (uncurry relationItemsComposable) 
                        $ liftA2 (,) r1 r2

permuteRelation :: Ord a => RelationItem a -> Relation b -> Relation b
permuteRelation p = fmap f
    where f ri = fmap snd $ RI.sortBy g $ RI.zip p ri
          g x y = compare (fst x) (fst y)


matrixToRelation :: [[a]] -> Relation a
matrixToRelation xs = Rel $ reverse $ map (RI.fromList) xs

relationToMatrix :: Relation a -> [[a]]
relationToMatrix (Rel xs) = reverse $ map toList xs

relationHasResults :: Relation a -> Bool
relationHasResults = not . null