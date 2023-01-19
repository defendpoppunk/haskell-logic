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


queryPredicate :: Eq a => RelationQuery a -> (RelationItem a -> Bool)
queryPredicate rq ri = foldr (&&) True $ RI.zipWith queryElementMatchesValue rq ri

truncateByQuery :: Relation a -> RelationQuery a -> Relation a
truncateByQuery r ri = fmap f r
    where f = flip truncateRelationItemByMask (queryVariableMask ri)

queryVariableMask :: RelationQuery a -> RelationItem Bool
queryVariableMask ri = fmap queryElementIsVariable ri

truncateRelationItemByMask :: RelationItem a -> RelationItem Bool -> RelationItem a
truncateRelationItemByMask ri vm = fmap fst $ RI.filter snd $ RI.zip ri vm


applyQuery :: Eq a => Relation a -> RelationQuery a -> Relation a
applyQuery r ri = truncateByQuery r' ri
    where r' = R.filter (queryPredicate ri) r