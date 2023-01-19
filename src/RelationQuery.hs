module RelationQuery
    ( QueryElement(Fixed, Variable)
    , RelationQuery
    , queryElementMatchesValue
    , queryElementIsVariable
    ) where

import RelationItem (RelationItem)


data QueryElement a = Fixed { fixedValue :: a } | Variable deriving (Show)

type RelationQuery a = RelationItem (QueryElement a)


queryElementMatchesValue :: Eq a => QueryElement a -> a -> Bool
queryElementMatchesValue (Variable) _ = True
queryElementMatchesValue (Fixed x) y = x == y

queryElementIsVariable :: QueryElement a -> Bool
queryElementIsVariable (Variable) = True
queryElementIsVariable (Fixed _) = False