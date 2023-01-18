module Logic
    ( permuteRelation
    , combineRelation
    , composeRelation
    , applyQuery
    , relationHasResults
    ) where

import Control.Monad
import Data.Foldable
import Data.Ord
import RelationItem hiding (filter, zip, sortBy)
import qualified RelationItem as RI
import Relation hiding (filter)
import qualified Relation as R
import RelationQuery


queryElementEquals :: Eq a => QueryElement a -> a -> Bool
queryElementEquals (Variable) _ = True
queryElementEquals (Fixed y) x = x == y

queryPredicate :: Eq a => RelationQuery a -> (RelationItem a -> Bool)
queryPredicate re ri = foldr (&&) True bs
    where bs = zipWith queryElementEquals (toList re) (toList ri)

isInRelation :: Eq a => Relation a -> (RelationItem a -> Bool)
isInRelation = flip elem

truncateByQuery :: Relation a -> RelationQuery a -> Relation a
truncateByQuery r ri = fmap f r
    where vm = queryVariableMask ri
          f re = truncateRelationItemByMask re vm

queryVariableMask :: RelationQuery a -> RelationItem Bool
queryVariableMask ri = fmap f ri
    where f (Variable) = True
          f (Fixed _) = False

truncateRelationItemByMask :: RelationItem a -> RelationItem Bool -> RelationItem a
truncateRelationItemByMask ri vm = fmap fst $ RI.filter snd $ RI.zip ri vm


applyQuery :: Eq a => Relation a -> RelationQuery a -> Relation a
applyQuery r ri = truncateByQuery r' ri
    where r' = R.filter (queryPredicate ri) r

relationHasResults :: Relation a -> Bool
relationHasResults = not . null

permuteRelation :: Ord a => RelationItem a -> Relation b -> Relation b
permuteRelation p = fmap f
    where f ri = fmap snd $ RI.sortBy g $ RI.zip p ri
          g x y = compare (fst x) (fst y)

composeRelation :: Eq a => Relation a -> Relation a -> Relation a
composeRelation r1 r2 = Rel $ map g xs
    where xs = filter f $ liftM2 (,) (toList r1) (toList r2)
          f (RelItem [], _         ) = True
          f (_         , RelItem []) = True
          f (RelItem xs, RelItem ys) = (last xs) == (head ys)
          g (ri        , RelItem []) = ri
          g (RelItem [], ri        ) = ri
          g (RelItem xs, RelItem ys) = RelItem $ (init xs) ++ (tail ys)

combineRelation :: Relation a -> Relation a -> Relation a
combineRelation r1 r2 = Rel $ (toList r1) ++ (toList r2)