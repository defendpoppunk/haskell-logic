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


data Relation' a = Rel { toChurchList :: ChurchList a } deriving (Show)
instance Foldable Relation' where
    foldr f x = foldr f x . toChurchList
instance Functor Relation' where
    fmap = churchListMap . fmap
instance Applicative Relation' where
    pure = Rel . pure
    (<*>) = churchListLiftA2 (<*>)
instance Semigroup (Relation' a) where
    (<>) = churchListLiftA2 (<>)

type Relation a = Relation' (RelationItem a)


churchListMap :: (ChurchList a -> ChurchList b) -> Relation' a -> Relation' b
churchListMap f r = Rel $ f $ toChurchList r

churchListLiftA2 :: (ChurchList a -> ChurchList b -> ChurchList c) -> Relation' a -> Relation' b -> Relation' c
churchListLiftA2 f r1 r2 = Rel $ f (toChurchList r1) (toChurchList r2)


filter :: (a -> Bool) -> Relation' a -> Relation' a
filter = churchListMap . CL.filter

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
matrixToRelation = fromList . reverse . map RI.fromList

relationToMatrix :: Relation a -> [[a]]
relationToMatrix = reverse . map toList . toList

relationHasResults :: Relation a -> Bool
relationHasResults = not . null


relationFail :: Relation a
relationFail = Rel $ mempty

relationTrue :: Relation a
relationTrue = Rel $ pure mempty

relationRepeat :: Relation a
relationRepeat = Rel $ CL.repeat mempty