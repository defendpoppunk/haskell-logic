module NewRelation
    ( NRelation(NRelation)
    , bindings
    , items
    , bindVariables
    , fromMatrix
    , disjunction
    , conjunction
    , (/\)
    , (\/)
    , nRelationTrue
    , nRelationFail
    , nRelationRepeat
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Applicative

data NRelation a b = NRelation { bindings :: [a], items :: [Map a b] }
instance (Show a, Show b) => Show (NRelation a b) where
    show (NRelation xs ys) = "NRelation: {" 
                             ++ "(" ++ (L.intercalate "," $ L.map show xs)  ++ ")"
                             ++ ": " ++ (L.intercalate ", " $ L.map f ys) ++ "}"
        where f m = "[" ++ (L.intercalate "," $ L.map g $ M.toList m) ++ "]"
              g (x, y) = (show x) ++ "=" ++ (show y)

-- TODO: bindVariables ["X", "X"] does not work as expected
bindVariables :: Ord a => [a] -> NRelation Int b -> NRelation a b
bindVariables xs (NRelation ys zs) = NRelation xs $ L.map (flip M.compose $ M.fromList $ zip xs ys) zs

disjunction :: Eq a => NRelation a b -> NRelation a b -> NRelation a b
disjunction (NRelation xs1 ys1) (NRelation xs2 ys2) = NRelation (L.union xs1 xs2) (ys1 ++ ys2)

conjunction :: (Ord a, Eq b) => NRelation a b -> NRelation a b -> NRelation a b
conjunction (NRelation xs1 ys1) (NRelation xs2 ys2) = NRelation (L.union xs1 xs2) ys3
    where ys3 = fmap     (uncurry M.union) 
                $ L.filter (uncurry itemsMergable) 
                $ liftA2 (,) ys1 ys2

(/\) :: (Ord a, Eq b) => NRelation a b -> NRelation a b -> NRelation a b
(/\) = conjunction

(\/) :: (Ord a, Eq b) => NRelation a b -> NRelation a b -> NRelation a b
(\/) = disjunction

itemsMergable :: (Ord a, Eq b) => Map a b -> Map a b -> Bool
itemsMergable m1 m2 = all id $ M.intersectionWith (==) m1 m2



fromMatrix :: [[a]] -> NRelation Int a
fromMatrix [] = NRelation [] []
fromMatrix (x:xs) = NRelation (L.take (length x) [0..]) $ L.map (M.fromList . zip [0..]) (x:xs)

nRelationTrue :: NRelation a b
nRelationTrue = NRelation [] [M.empty]

nRelationFail :: NRelation a b
nRelationFail = NRelation [] []

nRelationRepeat :: NRelation a b
nRelationRepeat = NRelation [] $ repeat M.empty
