module Relation
    ( Relation
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
    , relationNot
    ) where

import Prelude hiding (filter, repeat)
import qualified Prelude as P
import RelationItem hiding (filter, zip, sortBy, zipWith, fromList)
import qualified RelationItem as RI
import Data.Foldable (toList)
import Control.Applicative
import ChurchList (ChurchList(ChurchList), filter, repeat)
import qualified ChurchList as CL


type Relation a = ChurchList (RelationItem a)


combineRelation :: Relation a -> Relation a -> Relation a
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
matrixToRelation = CL.fromList . reverse . map RI.fromList

relationToMatrix :: Relation a -> [[a]]
relationToMatrix = reverse . map toList . toList

relationHasResults :: Relation a -> Bool
relationHasResults = not . null

relationNot :: Relation a -> Relation a
relationNot r
    | relationHasResults r = relationFail
    | otherwise            = relationTrue


relationFail :: Relation a
relationFail = mempty

relationTrue :: Relation a
relationTrue = pure mempty

relationRepeat :: Relation a
relationRepeat = repeat mempty