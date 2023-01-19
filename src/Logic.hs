module Logic
    ( applyQuery
    ) where

import Control.Monad
import Data.Foldable
import Data.Ord
import RelationItem hiding (filter, zip, sortBy, zipWith)
import qualified RelationItem as RI
import Relation hiding (filter)
import qualified Relation as R
import RelationQuery
import Control.Applicative


queryElementEquals :: Eq a => QueryElement a -> a -> Bool
queryElementEquals (Variable) _ = True
queryElementEquals (Fixed y) x = x == y

queryPredicate :: Eq a => RelationQuery a -> (RelationItem a -> Bool)
queryPredicate rq ri = foldr (&&) True $ RI.zipWith queryElementEquals rq ri

truncateByQuery :: Relation a -> RelationQuery a -> Relation a
truncateByQuery r ri = fmap f r
    where vm = queryVariableMask ri
          f ri = truncateRelationItemByMask ri vm

queryVariableMask :: RelationQuery a -> RelationItem Bool
queryVariableMask ri = fmap f ri
    where f (Variable) = True
          f (Fixed _) = False

truncateRelationItemByMask :: RelationItem a -> RelationItem Bool -> RelationItem a
truncateRelationItemByMask ri vm = fmap fst $ RI.filter snd $ RI.zip ri vm


applyQuery :: Eq a => Relation a -> RelationQuery a -> Relation a
applyQuery r ri = truncateByQuery r' ri
    where r' = R.filter (queryPredicate ri) r