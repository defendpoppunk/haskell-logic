module Logic
    ( Relation
    , Relation'(Rel)
    , QueryElement(Fixed, Variable)
    , RelationQuery'(RelQuery)
    , RelationQuery
    , RelationItem(RelItem)
    , permuteRelation
    , combineRelation
    , composeRelation
    , applyQuery
    , relationToList
    , listToRelation
    , relationHasResults
    ) where

import Control.Monad
import Data.Foldable
import Data.List
import Data.Ord

data RelationItem a = RelItem [a] deriving (Show, Eq)
instance Foldable RelationItem where
    foldr f x (RelItem xs) = foldr f x xs
instance Functor RelationItem where
    fmap f (RelItem xs) = RelItem $ fmap f xs

data Relation' a = Rel [a] deriving (Show, Eq)
instance Foldable Relation' where
    foldr f x (Rel xs) = foldr f x xs
instance Functor Relation' where
    fmap f (Rel xs) = Rel $ fmap f xs

data RelationQuery' a = RelQuery [a] deriving (Show, Eq)
instance Foldable RelationQuery' where
    foldr f x (RelQuery xs) = foldr f x xs
instance Functor RelationQuery' where
    fmap f (RelQuery xs) = RelQuery $ fmap f xs

data QueryElement a = Fixed { fixedValue :: a } | Variable deriving (Show, Eq)

type Relation a = Relation' (RelationItem a)

type RelationQuery a = RelationQuery' (QueryElement a)

type RelationPredicate a = RelationItem a -> Bool


listToRelation :: [[a]] -> Relation a
listToRelation xs = Rel $ reverse $ map (RelItem) xs

relationToList :: Relation a -> [[a]]
relationToList (Rel xs) = reverse $ map toList xs

queryElementEquals :: Eq a => QueryElement a -> a -> Bool
queryElementEquals (Variable) _ = True
queryElementEquals (Fixed y) x = x == y

queryPredicate :: Eq a => RelationQuery a -> RelationPredicate a
queryPredicate re ri = foldr (&&) True bs
    where bs = zipWith queryElementEquals (toList re) (toList ri)

isInRelation :: Eq a => Relation a -> RelationPredicate a
isInRelation = flip elem

relationFilter :: RelationPredicate a -> Relation a -> Relation a
relationFilter p r = Rel $ filter p (toList r)

truncateByQuery :: Relation a -> RelationQuery a -> Relation a
truncateByQuery r ri = fmap f r
    where m = queryVariableMask ri
          f re = truncateRelationItemByMask re m

queryVariableMask :: RelationQuery a -> [Bool]
queryVariableMask ri = toList $ fmap f ri
    where f (Variable) = True
          f (Fixed _) = False

truncateRelationItemByMask :: RelationItem a -> [Bool] -> RelationItem a
truncateRelationItemByMask re m = RelItem $ map fst $ filter snd $ zip (toList re) m

applyQuery :: Eq a => Relation a -> RelationQuery a -> Relation a
applyQuery r ri = truncateByQuery r' ri
    where r' = relationFilter (queryPredicate ri) r

relationHasResults :: Relation a -> Bool
relationHasResults = not . null

permuteRelation :: Ord a => [a] -> Relation b -> Relation b
permuteRelation p = fmap f
    where f ri = RelItem $ map snd $ sortBy g $ zip p (toList ri)
          g x y = compare (fst x) (fst y)

composeRelation :: Eq a => Relation a -> Relation a -> Relation a
composeRelation r1 r2 = Rel $ map g zs
    where zs = filter f $ liftM2 (,) (toList r1) (toList r2)
          f (RelItem [], _         ) = True
          f (_         , RelItem []) = True
          f (RelItem xs, RelItem ys) = (last xs) == (head ys)
          g (ri        , RelItem []) = ri
          g (RelItem [], ri        ) = ri
          g (RelItem xs, RelItem ys) = RelItem $ (init xs) ++ (tail ys)

combineRelation :: Relation a -> Relation a -> Relation a
combineRelation r1 r2 = Rel $ (toList r1) ++ (toList r2)