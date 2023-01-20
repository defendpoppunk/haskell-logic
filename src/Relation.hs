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
    , relationFail
    , relationTrue
    , relationRepeat
    , fromList
    ) where

import Prelude hiding (filter)
import qualified Prelude as P
import RelationItem hiding (filter, zip, sortBy, zipWith, fromList)
import qualified RelationItem as RI
import Data.Foldable (toList)
import Control.Applicative
import ChurchList (ChurchList(ChurchList))
import qualified ChurchList as CL


data Relation' a = Rel (ChurchList a) deriving (Show)
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
filter p r = Rel $ CL.fromList $ P.filter p (toList r)

fromList :: [a] -> Relation' a
fromList = Rel . CL.fromList


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
matrixToRelation xs = fromList $ reverse $ map (RI.fromList) xs

relationToMatrix :: Relation a -> [[a]]
relationToMatrix r = reverse $ map toList $ toList r

relationHasResults :: Relation a -> Bool
relationHasResults = not . null


relationFail :: Relation a
relationFail = Rel $ mempty

relationTrue :: Relation a
relationTrue = Rel $ pure mempty

relationRepeat :: Relation a
relationRepeat = fromList $ repeat mempty